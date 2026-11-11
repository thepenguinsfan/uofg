package eportfolio;

/**
 * Where Portfolio sends its messages and the total gain.
 * Added for the CIS*4150 term project so that Portfolio can be called from a
 * test without the Swing window: a test passes its own implementation to
 * Portfolio.setOutput and records what it receives.
 */
public interface PortfolioOutput {

    //show a message to the user
    void printMessage(String message);

    //show the total gain of the portfolio
    void setGainLabel(double totalGain);

    //default output: the Swing GUI, exactly as in the original program
    PortfolioOutput GUI = new PortfolioOutput() {
        public void printMessage(String message) {
            MainFrame.printMessage(message);
        }

        public void setGainLabel(double totalGain) {
            GainPanel.setGainLabel(totalGain);
        }
    };
}
