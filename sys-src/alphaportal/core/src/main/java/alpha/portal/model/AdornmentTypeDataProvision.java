package alpha.portal.model;

public enum AdornmentTypeDataProvision {
    OPEN("Offen"), INPROGRESS("In Arbeit"), FULLFILLED("Erfüllt");

    private final String name;

    private AdornmentValueType dataValueType;

    /**
     * Instantiates a new AdornmentType.
     * 
     * @param name
     *            the value of the specific adornment.
     */
    AdornmentTypeDataProvision(final String name) {
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
     * Getter for name (needed by jsp).
     */
    public String getName() {
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
    public static AdornmentTypeDataProvision fromValue(final String value) {
        for (final AdornmentTypeDataProvision adornmentTypeVisibility : AdornmentTypeDataProvision.values()) {
            if (adornmentTypeVisibility.name.equals(value))
                return adornmentTypeVisibility;
        }
        return null;
    }
}