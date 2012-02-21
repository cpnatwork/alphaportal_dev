package alpha.portal.model;

public enum AdornmentTypeDeleted {
    TRUE("Ja"), FALSE("Nein");

    private final String name;

    private AdornmentValueType dataValueType;

    /**
     * Instantiates a new AdornmentType.
     * 
     * @param name
     *            the value of the specific adornment.
     */
    AdornmentTypeDeleted(final String name) {
        this.name = name;
    }

    /**
     * gets the value of the enum list.
     * 
     * @return the value
     */
    public String value() {
        return name;
    }

    /**
     * goes through the list of default adornments and returns an object of the type AdornmentType, if the given value
     * equals with one out of the lists.
     * 
     * @param value
     *            the value of the adornment.
     * @return an AdornmentType object.
     */
    public static AdornmentTypeDeleted fromValue(final String value) {
        for (final AdornmentTypeDeleted adornmentTypeDeleted : AdornmentTypeDeleted.values()) {
            if (adornmentTypeDeleted.name.equals(value)) {
                return adornmentTypeDeleted;
            }
        }
        return null;
    }
}