package eportfolio;
import javax.swing.*;
import java.awt.*;
public class BuyPanel extends JPanel {

    public BuyPanel(){
        //set layout 
        setLayout(null);
        //create labels
        JLabel buyLabel = new JLabel("Buying an Investment");
        buyLabel.setBounds(20, 30, 200, 30);

        JLabel typeLabel = new JLabel("Type:");
        typeLabel.setBounds(40, 70, 100, 30);

        JLabel symbolLabel = new JLabel("Symbol:");
        symbolLabel.setBounds(40, 110, 100, 30);

        JLabel nameLabel = new JLabel("Name:");
        nameLabel.setBounds(40, 150, 100, 30);

        JLabel quantityLabel = new JLabel("Quantity:");
        quantityLabel.setBounds(40, 190, 100, 30);

        JLabel priceLabel = new JLabel("Price:");
        priceLabel.setBounds(40, 230, 100, 30);

        //create combo box
        JComboBox<String> typeComboBox = new JComboBox<>();
        typeComboBox.addItem("Stock");
        typeComboBox.addItem("Mutual Fund");
        typeComboBox.setBounds(150, 70, 200, 30);

        //create text fields
        JTextField symbolTextField = new JTextField();
        symbolTextField.setBounds(150, 110, 140, 30);

        JTextField nameTextField = new JTextField();
        nameTextField.setBounds(150, 150, 200, 30);

        JTextField quantityTextField = new JTextField();
        quantityTextField.setBounds(150, 190, 140, 30);

        JTextField priceTextField = new JTextField();
        priceTextField.setBounds(150, 230, 140, 30);
        
        //create buttons
        JButton buyButton = new JButton("Buy");
        buyButton.setBounds(450, 190, 140, 30);

        JButton resetButton = new JButton("Reset");
        resetButton.setBounds(450, 70, 140, 30);
        //add components to panel
        add(buyLabel);
        add(typeLabel);
        add(symbolLabel);
        add(nameLabel);
        add(quantityLabel);
        add(priceLabel);
        add(typeComboBox);
        add(symbolTextField);
        add(nameTextField);
        add(quantityTextField);
        add(priceTextField);
        add(buyButton);
        add(resetButton);

        //reset button action listener
        resetButton.addActionListener(e -> {
            typeComboBox.setSelectedIndex(0);
            symbolTextField.setText("");
            nameTextField.setText("");
            quantityTextField.setText("");
            priceTextField.setText("");
        });
        //buy button action listener
        buyButton.addActionListener(e ->{
            Portfolio.buyInvestment(typeComboBox.getSelectedItem().toString(), symbolTextField.getText(), nameTextField.getText(), quantityTextField.getText(), priceTextField.getText());
        });
    }
}
