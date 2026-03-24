      *> Luc Levesque
      *> 1238403
      *> statmold.cob
      *> legacy statistics program

       identification division.
       program-id. statmold.

       environment division.
       input-output section.
       file-control.
           select input-file assign to "nums.txt"
               organization is line sequential.
           select output-file assign to "output.txt"
               organization is line sequential.

       data division.
       file section.
       fd input-file.
       01 input-record pic x(80).
       fd output-file.
       01 output-line pic x(80).

       working-storage section.
       77 sum-of-x-sqr pic 9(14)v9(2).
       77 sum-of-x pic s9(10)v9(2).
       77 n pic s9(4).
       77 mean pic s9(6)v9(2).
       77 i pic s9(4).
      *> data value storage array
       01 array-area.
           02 x pic s9(6)v9(2) occurs 1000 times.
      *> input record: numeric value followed by padding
       01 input-value-record.
           02 in-x pic s9(6)v9(2).
           02 filler pic x(72).
      *> formatted output line definitions
       01 output-title-line.
           02 filler pic x(28) value
               " MEAN AND STANDARD DEVIATION".
       01 output-underline.
           02 filler pic x(28) value
               "----------------------------".
       01 output-col-heads.
           02 filler pic x(10) value spaces.
           02 filler pic x(11) value "DATA VALUES".
       01 output-data-line.
           02 filler pic x(10) value spaces.
           02 out-x pic -(6)9.9(2).
       01 output-results-line-1.
           02 filler pic x(9) value " MEAN=   ".
           02 out-mean pic -(6)9.9(2).
       01 output-results-line-2.
           02 filler pic x(9) value " STD DEV=".
           02 std-deviation pic -(6)9.9(2).

       procedure division.
           open input input-file, output output-file.
           move zero to in-x.
      *> loop until sentinel value 99999999 is encountered
           perform proc-body
               until in-x is equal to 999999.99.
           perform end-of-job.
           stop run.

      *> main processing paragraph
       proc-body.
           write output-line from output-title-line
               after advancing 1 line.
           write output-line from output-underline
               after advancing 1 line.
           write output-line from output-col-heads
               after advancing 1 line.
           write output-line from output-underline
               after advancing 1 line.
           move zero to sum-of-x.
           read input-file into input-value-record
               at end perform end-of-job.
      *> collect values up to sentinel or array limit
           perform input-loop
               varying n from 1 by 1
               until n is greater than 1000
               or in-x is not less than 999999.98.
      *> adjust count since varying increments past last value
           subtract 1 from n.
           divide n into sum-of-x giving mean rounded.
           move zero to sum-of-x-sqr.
      *> second pass for squared differences
           perform sum-loop
               varying i from 1 by 1
               until i is greater than n.
           compute std-deviation rounded =
               (sum-of-x-sqr / n) ** 0.5.
           write output-line from output-underline
               after advancing 1 line.
           move mean to out-mean.
           write output-line from output-results-line-1
               after advancing 1 line.
           write output-line from output-results-line-2
               after advancing 1 line.

      *> process one input value and read the next
       input-loop.
           move in-x to x(n), out-x.
           write output-line from output-data-line
               after advancing 1 line.
           add x(n) to sum-of-x.
           read input-file into input-value-record
               at end perform end-of-job.

      *> add (x(i) - mean) ** 2 to running total
       sum-loop.
           compute sum-of-x-sqr =
               sum-of-x-sqr + (x(i) - mean) ** 2.

      *> close files and stop
       end-of-job.
           close input-file, output-file.
           stop run.
