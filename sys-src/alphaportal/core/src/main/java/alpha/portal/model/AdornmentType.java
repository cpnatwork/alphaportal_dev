package alpha.portal.model;

/**
 * Adornment type is a list of default adornments, which will be generated, if a new α-Card is created.
 */

public enum AdornmentType {

    /**
     * The adornment "Title", which defines the name of the adornment.
     */
    Title("AlphaCard Titel", "no Titel"),
    /**
     * The adornment "Contributor", which defines the responsible person of this α-Card.
     */
    Contributor("Verantwortlicher", AdornmentValueType.Enum),
    /**
     * The adornment "ContributorRole", which defines the role of the responsible person of this α-Card.
     */
    ContributorRole("Fachgebiet des Verantwortlichen", AdornmentValueType.Enum),

    /**
     * the adornment "Visibility", which defines the visibility of the payload of this α-Card.
     */
    Visibility("Sichtbarkeit", AdornmentValueType.Enum, AdornmentTypeVisibility.PRIVATE.value(),
            new AdornmentTypeRange(new String[] { AdornmentTypeVisibility.PRIVATE.value(),
                    AdornmentTypeVisibility.PUBLIC.value() })),

    /**
     * the adornment "Validity", which defines the validity of the payload of this α-Card.
     */
    Validity("Gültigkeit", AdornmentValueType.Enum, AdornmentTypeValidity.INVALID.value(), new AdornmentTypeRange(
            new String[] { AdornmentTypeValidity.VALID.value(), AdornmentTypeValidity.INVALID.value() })),

    /**
     * the adornment "Deleted", which defines if this α-Card was deleted.
     */
    Deleted("Gelöscht", AdornmentValueType.Enum, AdornmentTypeDeleted.FALSE.value(), new AdornmentTypeRange(
            new String[] { AdornmentTypeDeleted.FALSE.value(), AdornmentTypeDeleted.TRUE.value() })),

    /**
     * the adornment "Data Provision", which defines the provision of the payload of this α-Card.
     */
    DataProvision("Zustand", AdornmentValueType.Enum, AdornmentTypeDataProvision.OPEN.value(), new AdornmentTypeRange(
            new String[] { AdornmentTypeDataProvision.OPEN.value(), AdornmentTypeDataProvision.INPROGRESS.value(),
                    AdornmentTypeDataProvision.FULLFILLED.value() })),

    /**
     * The adornment "Title", which defines the name of the adornment.
     */
    PayloadVersionNumber("Payload Versionsnummer", AdornmentValueType.Integer, "0", new AdornmentTypeRange(0,
            Integer.MAX_VALUE));

    /**
     * The value of the specific adornment.
     */
    private final String name;

    /**
     * the types a value can be (Integer, Float, String or Enum)
     */
    private AdornmentValueType valueType;

    /**
     * the range a value can be
     */
    private AdornmentTypeRange valueRange;

    /**
     * the defaultValue of the adornment.
     */
    private String valueDefault;

    /**
     * Instantiates a new AdornmentType.
     * 
     * @param name
     *            the value of the specific adornment.
     */
    AdornmentType(final String name) {
        this(name, AdornmentValueType.String);
    }

    /**
     * Instantiates a new AdornmentType with valueType.
     * 
     * @param name
     *            the value of the specific adornment.
     */
    AdornmentType(final String name, final AdornmentValueType type) {
        this(name, type, "");
    }

    /**
     * Instantiates a new AdornmentType.
     * 
     * @param name
     *            the value of the specific adornment.
     */
    AdornmentType(final String name, final String defaultValue) {
        this(name, AdornmentValueType.String, defaultValue);
    }

    /**
     * Instantiates a new AdornmentType.
     * 
     * @param name
     *            the value of the specific adornment.
     */
    AdornmentType(final String name, final AdornmentValueType type, final String defaultValue) {
        this(name, type, defaultValue, null);
    }

    /**
     * Instantiates a new AdornmentType.
     * 
     * @param name
     *            the value of the specific adornment.
     * @param range
     *            the valid range of the adornment values.
     * @param defaultValue
     *            the default value of the adornment.
     */
    AdornmentType(final String name, final AdornmentValueType type, final String defaultValue,
            final AdornmentTypeRange range) {
        this.name = name;
        this.valueType = type;
        this.valueDefault = defaultValue;
        this.valueRange = range;
    }

    /**
     * gets the name of the enum list.
     * 
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * goes through the list of default adornments and returns an object of the type AdornmentType, if the given value
     * equals with one out of the lists.
     * 
     * @param name
     *            the value of the adornment.
     * @return an AdornmentType object.
     */
    public static AdornmentType fromName(final String name) {
        for (final AdornmentType adornmentType : AdornmentType.values()) {
            if (adornmentType.name.equals(name)) {
                return adornmentType;
            }
        }
        return null;
    }

    public AdornmentValueType getValueType() {
        return valueType;
    }

    public void setValueType(final AdornmentValueType valueType) {
        this.valueType = valueType;
    }

    public AdornmentTypeRange getValueRange() {
        return valueRange;
    }

    public void setValueRange(final AdornmentTypeRange valueRange) {
        this.valueRange = valueRange;
    }

    public String getValueDefault() {
        return valueDefault;
    }

    public void setValueDefault(final String valueDefault) {
        this.valueDefault = valueDefault;
    }

    public boolean validate(final String value) {
        if (valueRange == null) {
            return true;
        }
        switch (this.valueType) {
        case Enum:
            return valueRange.isValid(value);
        case Float:
            try {
                return valueRange.isValid(Float.parseFloat(value));
            } catch (NumberFormatException e) {
                return false;
            }
        case Integer:
            try {
                return valueRange.isValid(Integer.parseInt(value));
            } catch (NumberFormatException e) {
                return false;
            }
        case String:
            return true;
        }
        return false;
    }
}
