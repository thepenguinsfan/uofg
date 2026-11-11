package eportfolio;
import javax.swing.*;
import java.awt.*;
public class GainPanel extends JPanel{
    private static JTextField totalGainTextField;
    public GainPanel(){
        //set layout
        setLayout(null);
        //create labels
        JLabel gainLabel = new JLabel("Getting total gain");
        gainLabel.setBounds(20, 30, 200, 30);

        JLabel totalGainLabel = new JLabel("Total Gain");
        totalGainLabel.setBounds(40, 70, 100, 30);
        //create button
        JButton getGainButton = new JButton("Get Gain");
        getGainButton.setBounds(450, 190, 140, 30);
        //create text field
        totalGainTextField = new JTextField();
        totalGainTextField.setBounds(150, 70, 100, 30);
        totalGainTextField.setEditable(false);
        //add components to panel
        add(gainLabel);
        add(totalGainLabel);
        add(totalGainTextField);
        add(getGainButton);

        //get gain button action listener
        getGainButton.addActionListener(e -> {
            Portfolio.getGain();
        });
    }
    //set gain text field
    public static void setGainLabel(double gain){
        totalGainTextField.setText(String.format("%.2f", gain));
    }
}
