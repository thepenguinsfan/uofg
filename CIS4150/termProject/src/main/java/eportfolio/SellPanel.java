package eportfolio;

import javax.swing.*;
import java.awt.*;

public class SellPanel extends JPanel{
    public SellPanel(){
        //set layout
        setLayout(null);
        //create labels
        JLabel sellLabel = new JLabel("Selling an Investment");
        sellLabel.setBounds(20, 30, 200, 30);

        JLabel symbolLabel = new JLabel("Symbol");
        symbolLabel.setBounds(40, 70, 100, 30);

        JLabel quantityLabel = new JLabel("Quantity");
        quantityLabel.setBounds(40, 150, 100, 30);

        JLabel priceLabel = new JLabel("Price");
        priceLabel.setBounds(40, 230, 100, 30);
        //create text fields
        JTextField symbolTextField = new JTextField();
        symbolTextField.setBounds(150, 70, 150, 30);

        JTextField quantityTextField = new JTextField();
        quantityTextField.setBounds(150, 150, 100, 30);

        JTextField priceTextField = new JTextField();
        priceTextField.setBounds(150, 230, 100, 30);
        //create buttons
        JButton sellButton = new JButton("Sell");
        sellButton.setBounds(450, 190, 140, 30);

        JButton resetButton = new JButton("Reset");
        resetButton.setBounds(450, 70, 140, 30);
        //add components to panel
        add(sellLabel);
        add(symbolLabel);
        add(quantityLabel);
        add(priceLabel);
        add(symbolTextField);
        add(quantityTextField);
        add(priceTextField);    
        add(sellButton);
        add(resetButton);

        //reset button action listener
        resetButton.addActionListener(e -> {
            symbolTextField.setText("");
            quantityTextField.setText("");
            priceTextField.setText("");
        });

        //sell button action listener
        sellButton.addActionListener(e ->{
            Portfolio.sellInvestment(symbolTextField.getText(), quantityTextField.getText(), priceTextField.getText());
        });
    }
}

