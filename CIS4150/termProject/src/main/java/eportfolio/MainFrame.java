package eportfolio;
import javax.swing.*;
import java.awt.*;
public class MainFrame extends JFrame {
    private static JTextArea messagesArea;
    public MainFrame(String fileName){
        super("ePortfolio");

        //create frame
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);//CHANGE
        setSize(800, 600);
        setLayout(new BorderLayout());

        //message panel
        messagesArea = new JTextArea();
        messagesArea.setEditable(false);
        JScrollPane messagesScrollPane = new JScrollPane(messagesArea);
        messagesScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        messagesScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);


        //card layout for top panel
        JPanel topPanels = new JPanel(new CardLayout());
        topPanels.add(new WelcomePanel(), "Welcome");
        topPanels.add(new BuyPanel(), "Buy");
        topPanels.add(new SellPanel(), "Sell");
        topPanels.add(new UpdatePanel(), "Update");
        topPanels.add(new GainPanel(), "Gain");
        topPanels.add(new SearchPanel(), "Search");

        //menu bar
        JMenu menu = new JMenu("Commands");
        JMenuItem buyMenuItem = new JMenuItem("Buy");
        JMenuItem sellMenuItem = new JMenuItem("Sell");
        JMenuItem updateMenuItem = new JMenuItem("Update");
        JMenuItem gainMenuItem = new JMenuItem("Gain");
        JMenuItem searchMenuItem = new JMenuItem("Search");
        JMenuItem quitMenuItem = new JMenuItem("Quit");

        //add menu items to menu
        menu.add(buyMenuItem);
        menu.add(sellMenuItem);
        menu.add(updateMenuItem);
        menu.add(gainMenuItem);
        menu.add(searchMenuItem);
        menu.add(quitMenuItem);

        //add menu to menu bar
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(menu);
        setJMenuBar(menuBar);

        //action listeners for menu items
        buyMenuItem.addActionListener(e -> changePanel(topPanels, "Buy"));
        sellMenuItem.addActionListener(e -> changePanel(topPanels, "Sell"));
        updateMenuItem.addActionListener(e -> changePanel(topPanels, "Update"));
        gainMenuItem.addActionListener(e -> changePanel(topPanels, "Gain"));
        searchMenuItem.addActionListener(e -> changePanel(topPanels, "Search"));
        quitMenuItem.addActionListener(e -> {
            Portfolio.writeToFile(fileName);
            System.exit(0);
        });

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPanels, messagesScrollPane);
        splitPane.setDividerLocation(300);
        add(splitPane, BorderLayout.CENTER);

        //set visible
        setVisible(true);
        //add window listener
        String finalFileName = fileName;
        addWindowListener(new java.awt.event.WindowAdapter() {
            @Override
            public void windowClosing(java.awt.event.WindowEvent e) {
                Portfolio.writeToFile(finalFileName);
                System.exit(0);
            }
        });
        
    }

    //change panel method
    private static void changePanel(JPanel panel, String panelName){
        CardLayout cardLayout = (CardLayout) panel.getLayout();
        cardLayout.show(panel, panelName);
    }

    //print message to the message area
    //helper method
    public static void printMessage(String message){
        messagesArea.append(message + "\n");
    }
}
