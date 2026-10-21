package eportfolio;
import javax.swing.*;
import java.awt.*;
public class SearchPanel extends JPanel {
    public SearchPanel(){
        //set layout
        setLayout(null);
        //create labels
        JLabel searchLabel = new JLabel("Searching Investments");
        searchLabel.setBounds(20, 30, 200, 30);

        JLabel symbolLabel = new JLabel("Symbol");
        symbolLabel.setBounds(40, 70, 100, 30);

        JLabel nameLabel = new JLabel("Name");
        nameLabel.setBounds(40, 105, 100, 30);

        JLabel keyWordsLabel = new JLabel("Keywords");
        keyWordsLabel.setBounds(40, 125, 100, 30);

        JLabel lowPriceLabel = new JLabel("Low Price");
        lowPriceLabel.setBounds(40, 160, 100, 30);

        JLabel highPriceLabel = new JLabel("High Price");
        highPriceLabel.setBounds(40, 200, 100, 30);
        //create textfields 
        JTextField symbolTextField = new JTextField();
        symbolTextField.setBounds(150, 70, 150, 30);

        JTextField nameTextField = new JTextField();
        nameTextField.setBounds(150, 115, 200, 30);

        JTextField lowPriceTextField = new JTextField();
        lowPriceTextField.setBounds(150, 160, 100, 30);

        JTextField highPriceTextField = new JTextField();
        highPriceTextField.setBounds(150, 200, 100, 30);
        //create buttons
        JButton searchButton = new JButton("Search");
        searchButton.setBounds(450, 190, 140, 30);

        JButton resetButton = new JButton("Reset");
        resetButton.setBounds(450, 70, 140, 30);
        //add components to panel
        add(searchLabel);
        add(symbolLabel);
        add(nameLabel);
        add(keyWordsLabel);
        add(lowPriceLabel);
        add(highPriceLabel);
        add(symbolTextField);
        add(nameTextField);
        add(lowPriceTextField);
        add(highPriceTextField);
        add(searchButton);
        add(resetButton);

        //reset button action listener
        resetButton.addActionListener(e -> {
            symbolTextField.setText("");
            nameTextField.setText("");
            lowPriceTextField.setText("");
            highPriceTextField.setText("");
        });


        //search button action listener
        searchButton.addActionListener(e ->{
            Portfolio.search(symbolTextField.getText(), nameTextField.getText(), lowPriceTextField.getText(), highPriceTextField.getText());
        });
    }
    
}
