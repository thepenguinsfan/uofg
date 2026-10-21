package eportfolio;
import javax.swing.*;
import java.awt.*;
public class WelcomePanel extends JPanel{
    
    public WelcomePanel(){
        //set layout
        setLayout(null);
        //create labels
        JLabel welcomeLabel = new JLabel("Welcome to ePortfolio");
        welcomeLabel.setBounds(250, 50, 200, 30);

        JLabel instructionsLabel1 = new JLabel("Choose a command from the commands menu to buy or sell an investment,");
        instructionsLabel1.setBounds(50, 150, 550, 30);
        JLabel instructionsLabel2 = new JLabel("update prices for all investments, get gain for the portfolio,");
        instructionsLabel2.setBounds(50, 165, 550, 30);
        JLabel instructionsLabel3 = new JLabel("search for relevant investments, or quit the program.");
        instructionsLabel3.setBounds(50, 180, 550, 30);
        //add components to panel
        add(welcomeLabel);
        add(instructionsLabel1);
        add(instructionsLabel2);
        add(instructionsLabel3);
    }
}
