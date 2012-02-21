package alpha.portal.model;

import org.apache.commons.lang.ArrayUtils;

public class AdornmentTypeRange {
    private int maxInteger;
    private int minInteger;
    private float maxFloat;
    private float minFloat;
    private String[] validStrings;

    public AdornmentTypeRange(final int min, final int max) {
        this.maxInteger = max;
        this.minInteger = min;
    }

    public AdornmentTypeRange(final float min, final float max) {
        this.maxFloat = max;
        this.minFloat = min;
    }

    public AdornmentTypeRange(final String[] validStrings) {
        setValidStrings(validStrings);
    }

    public boolean isValid(final int value) {
        if (value <= maxInteger && value >= minInteger) {
            return true;
        }

        return false;
    }

    public boolean isValid(final float value) {
        if (value <= maxFloat && value >= minFloat) {
            return true;
        }

        return false;
    }

    public boolean isValid(final String value) {
        return ArrayUtils.contains(validStrings, value);
    }

    public int getMaxInteger() {
        return maxInteger;
    }

    public void setMaxInteger(final int maxInteger) {
        this.maxInteger = maxInteger;
    }

    public int getMinInteger() {
        return minInteger;
    }

    public void setMinInteger(final int minInteger) {
        this.minInteger = minInteger;
    }

    public float getMaxFloat() {
        return maxFloat;
    }

    public void setMaxFloat(final float maxFloat) {
        this.maxFloat = maxFloat;
    }

    public float getMinFloat() {
        return minFloat;
    }

    public void setMinFloat(final float minFloat) {
        this.minFloat = minFloat;
    }

    public String[] getValidStrings() {
        return (String[]) ArrayUtils.clone(validStrings);
    }

    public void setValidStrings(final String[] validStrings) {
        this.validStrings = (String[]) ArrayUtils.clone(validStrings);
    }

}
