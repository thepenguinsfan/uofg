package eportfolio;
import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
public class UpdatePanel extends JPanel{
    private static ArrayList<Investment> investments = Portfolio.getInvestmentList();
    private static JTextField symbolTextField;
    private static JTextField nameTextField;
    private static JTextField priceTextField;
    private static int currentIndex = 0;
    public UpdatePanel(){
        //set layout
        setLayout(null);
        //create labels
        JLabel updateLabel = new JLabel("Updating Invesments");
        updateLabel.setBounds(20, 30, 200, 30);

        JLabel symbolLabel = new JLabel("Symbol");
        symbolLabel.setBounds(40, 70, 100, 30);

        JLabel nameLabel = new JLabel("Name");
        nameLabel.setBounds(40, 150, 100, 30);

        JLabel priceLabel = new JLabel("Price");
        priceLabel.setBounds(40, 230, 100, 30);

        //create text fields
        symbolTextField = new JTextField();
        symbolTextField.setBounds(150, 70, 150, 30);
        symbolTextField.setEditable(false);

        nameTextField = new JTextField();
        nameTextField.setBounds(150, 150, 250, 30);
        nameTextField.setEditable(false);

        priceTextField = new JTextField();
        priceTextField.setBounds(150, 230, 100, 30);
        //create buttons
        JButton saveButton = new JButton("Save");
        saveButton.setBounds(450, 230, 140, 30);

        JButton prevButton = new JButton("Prev");
        prevButton.setBounds(450, 70, 140, 30); 

        JButton nextButton = new JButton("Next");
        nextButton.setBounds(450, 150, 140, 30);
        //add components to panel
        add(updateLabel);
        add(symbolLabel);
        add(nameLabel);
        add(priceLabel);
        add(symbolTextField);
        add(nameTextField);
        add(priceTextField);
        add(saveButton);
        add(prevButton);
        add(nextButton);
        //display first investment
        if(investments.size() > 0){
            displayInvestment(0);
        }
        else{
            symbolTextField.setText("No Investments");
            nameTextField.setText("No Investments");
        }
        //save button action listener
        saveButton.addActionListener(e -> {
            saveInvestment();
        });
        //prev button action listener
        prevButton.addActionListener(e -> {
            showPrevInvestment();
        });

        //next button action listener
        nextButton.addActionListener(e -> {
            showNextInvestment();
        });
    }
    //display investment
    private void displayInvestment(int index){
        Investment investment = investments.get(index);
        symbolTextField.setText(investment.getSymbol());
        nameTextField.setText(investment.getName());
    }
    //show next investment
    private void showNextInvestment(){
        currentIndex = (currentIndex + 1) % investments.size();
        displayInvestment(currentIndex);
    }
    //show previous investment
    private void showPrevInvestment(){
        currentIndex = (currentIndex - 1 + investments.size()) % investments.size();
        displayInvestment(currentIndex);
    }
    //save investment
    private void saveInvestment(){
        String price = priceTextField.getText();
        String symbol = symbolTextField.getText();
        Portfolio.updatePrice(symbol, price);
    }
}
