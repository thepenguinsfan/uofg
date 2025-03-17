
/*
Luc Levesque
ID: 1238403
*/

/*
 * The program simulates a Token Ring LAN by forking off a process
 * for each LAN node, that communicate via shared memory, instead
 * of network cables. To keep the implementation simple, it jiggles
 * out bytes instead of bits.
 *
 * It keeps a count of packets sent and received for each node.
 */
#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <stdbool.h>
#include <pthread.h>
#include "tokenRing.h"


#define ACTIVE_TOKEN '1'
#define INACTIVE_TOKEN '0'
#define SENDER_NODE '0'
#define NOT_SENDER_NODE '1'

typedef struct tokenArguements{
	int node;
	struct TokenRingData *control;
}tokenArguements;
/*
 * This function is the body of a child process emulating a node.
 */
void*
token_node(void *args)
{				
	tokenArguements *arg = (tokenArguements *)args;
	int num = arg->node;
	struct TokenRingData *control = arg->control;										
	int rcv_state = TOKEN_FLAG, not_done = 1, sending = 0, len;;
	unsigned char byte;
	bool isSender = false;
	/*
	 * If this is node #0, start the ball rolling by creating the
	 * token.
	 */
	if (num == 0) {
		send_byte(control, num, INACTIVE_TOKEN);
	}

	/*
	 * Loop around processing data, until done.
	 */
	while (not_done) {
		//receive a byte from previous node
		byte = rcv_byte(control, num);

		/*
		 * Handle the byte, based upon current state.
		 */
		switch (rcv_state) {
		case TOKEN_FLAG:
		    //wait for critical section
			WAIT_SEM(control, CRIT);
			//if the token is unused and there is data to send
			if ((byte == INACTIVE_TOKEN)
				&& (control->shared_ptr->node[num].to_send.length > 0)) {
				isSender = true;
			}
			else {
				isSender = false;
			}
			//signal end of critical section
			SIGNAL_SEM(control, CRIT);
			//if the token is unused and there is data to send
			if (byte == INACTIVE_TOKEN) {
				if (isSender) {
					//set the token flag to used
					control->snd_state = TOKEN_FLAG;
					//send the packet
					send_pkt(control, num);	
					//set the state to TO
					rcv_state = TO;
				}
				else {
					//wait for critical section
					WAIT_SEM(control, CRIT);
					//if the node is to terminate
					if (control->shared_ptr->node[num].terminate == 1) {
						not_done = 0;
					}
					//signal end of critical section
					SIGNAL_SEM(control, CRIT);
					//send the byte
					send_byte(control, num, byte);
					//set the state to TOKEN_FLAG
					rcv_state = TOKEN_FLAG;
				}
			}
			//if the token is used
			else {
				//send the byte
				send_byte(control, num, byte);
				//set the state to TO
				rcv_state = TO;
			}
			break;

		case TO:
			if (isSender) {
				send_pkt(control, num);
			}
			else {
				send_byte(control, num, byte);
			}
			rcv_state = FROM;	

			break;

		case FROM:
			//if this node is the sender
			if (isSender) {
				//send the packet
				send_pkt(control, num);
			}
			//if this node is not the sender
			else {
				//send the byte
				send_byte(control, num, byte);
			}
			//set the state to LEN
			rcv_state = LEN;
			break;

		case LEN:
			//if this node is the sender
			if (isSender) {
				//send the packet
				send_pkt(control, num);
				//wait for critical section
				WAIT_SEM(control, CRIT);
				//get the length of the data to send
				len = control->shared_ptr->node[num].to_send.length;
				//signal end of critical section
				SIGNAL_SEM(control, CRIT);
			}
			else {
				//send the byte
				send_byte(control, num, byte);
				//set the length of the data to the recieved byte
				len = (int) byte;
			}
			//reset the number of bytes sent
			sending = 0;

			//set the state to DATA
			if (len > 0) {
				rcv_state = DATA;
			}
			else {
				rcv_state = TOKEN_FLAG;
			}

			break;

		case DATA:
			//if data is still being sent
			if (sending < (len-1)) {
				//if this node is the sender
				if (isSender) {
					//send the packet
					send_pkt(control, num);
				}
				else {
					//send the byte
					send_byte(control, num, byte);
				}	
				//set the state to DATA
				rcv_state = DATA;
			}
			//if all data has been sent
			else {
				//if this node is the sender
				if (isSender) {
					//send the packet
					send_pkt(control, num);
					//wait for critical section
					WAIT_SEM(control, CRIT);
					//indicate that there is no more data to send
					control->shared_ptr->node[num].to_send.length = 0;
					//signal end of critical section
					SIGNAL_SEM(control, CRIT);
				}
				else {
					//send the byte
					send_byte(control, num, byte);
				}
				//this node is no longer the sender
				isSender = false;
				//set the state to TOKEN_FLAG
				rcv_state = TOKEN_FLAG;
			}
			sending++;
			break;
		}
	}
	free(arg);
	return NULL;
}

/*
 * This function sends a data packet followed by the token, one byte each
 * time it is called.
 */
void
send_pkt(control, num)
	struct TokenRingData *control;
	int num;
{
	static int sendPosition, sendLength, nodeNum;

	switch (control->snd_state) {
	case TOKEN_FLAG:
		//wait for critical section
		WAIT_SEM(control, CRIT);
		//increment the number of packets sent
		control->shared_ptr->node[num].sent = control->shared_ptr->node[num].sent + 1;
		//get the destination node number
		nodeNum = (int) control->shared_ptr->node[num].to_send.to;
		//increment the number of packets received by the destination node
		control->shared_ptr->node[nodeNum].received++;
		//set the token flag to used
		control->shared_ptr->node[num].to_send.token_flag = ACTIVE_TOKEN;
		//signal end of critical section
		SIGNAL_SEM(control, CRIT);
		//send destination node number
		send_byte(control, num, control->shared_ptr->node[num].to_send.token_flag);
		//set the state to TO
		control->snd_state = TO;
		//reset the position
		sendPosition = 0;
		//wait for critical section
		WAIT_SEM(control, CRIT);
		//get the length of the data to send
		sendLength = control->shared_ptr->node[num].to_send.length;
		//signal end of critical section
		SIGNAL_SEM(control, CRIT);
		break;

	case TO:
		//send the destination node number
		send_byte(control, num, control->shared_ptr->node[num].to_send.to);
		//set the state to FROM
		control->snd_state = FROM;
		break;

	case FROM:
		//send the source node number
		send_byte(control, num, control->shared_ptr->node[num].to_send.from);
		//set the state to LEN
		control->snd_state = LEN;
		break;

	case LEN:
		//send the length of the data to send
		send_byte(control, num, control->shared_ptr->node[num].to_send.length);
		//set the state to DATA
		control->snd_state = DATA;
		break;

	case DATA:	
		//if there is still data to send
		if (sendPosition < (sendLength-1)) {
			//send the data
			send_byte(control, num, control->shared_ptr->node[num].to_send.data[sendPosition]);
			//wait for critical section
			WAIT_SEM(control, CRIT);
			//increment the position
			sendPosition++;
			//set the state to DATA
			control->snd_state = DATA;
			//signal end of critical section
			SIGNAL_SEM(control, CRIT);
			break;
		}
		//if all data has been sent
		else {	
			//wait for critical section
			WAIT_SEM(control, CRIT);
			//set the state to DONE
			control->snd_state = DONE;
			//signal end of critical section
			SIGNAL_SEM(control, CRIT);
		}

	case DONE:
		//wait for critical section
		WAIT_SEM(control, CRIT);
		//set the token flag to not sender
		control->shared_ptr->node[num].to_send.token_flag = NOT_SENDER_NODE;
		//set the state to TOKEN_FLAG
		control->snd_state = TOKEN_FLAG;
		//semd unsusued token byte
		send_byte(control, num, INACTIVE_TOKEN);
		//signal the TO_SEND semaphore
		SIGNAL_SEM(control, TO_SEND(num));
		//signal end of critical section
		SIGNAL_SEM(control, CRIT);
		break;
	};
}

/*
 * Send a byte to the next node on the ring.
 */
void
send_byte(control, num, byte)
	struct TokenRingData *control;
	int num;
	unsigned byte;
{
	//wait for the next node to be empty
	WAIT_SEM(control, EMPTY(num));
	//send the byte to shared memory
	control->shared_ptr->node[num].data_xfer = byte;
	//signal the filled semaphore for this node
	SIGNAL_SEM(control, FILLED(num));
}

/*
 * Receive a byte for this node.
 */
unsigned char
rcv_byte(control, num)
	struct TokenRingData *control;
	int num;
{
	unsigned char byte;
	//node number to receive from
	int rcv_from;
	//calculate the node number to receive from
	rcv_from = (num + (N_NODES-1))%N_NODES;
	//wait for the node to be filled
	WAIT_SEM(control, FILLED(rcv_from));
	//receive the byte from shared memory
	byte = control->shared_ptr->node[rcv_from].data_xfer;
	//signal the empty semaphore for the node
	SIGNAL_SEM(control, EMPTY(rcv_from));
	return byte;
}
