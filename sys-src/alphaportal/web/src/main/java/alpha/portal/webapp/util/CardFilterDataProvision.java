package alpha.portal.webapp.util;

public enum CardFilterDataProvision {
    ALL("--Zustand"), OPEN("Offen"), NOTFULFILLED("Offen & in Arbeit"), INPROGRESS("In Arbeit"), FULLFILLED("Erfüllt");

    private final String name;

    /**
     * Instantiates a new CardFilterDataProvision.
     * 
     * @param name
     *            the value of the specific DataProvision.
     */
    CardFilterDataProvision(final String name) {
        this.name = name;
    }

    /**
     * Getter for name (needed by jsp).
     */
    public String getName() {
        return name;
    }
}
