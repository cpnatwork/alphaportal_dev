package alpha.portal.model;

public class AdornmentRules {

    /**
     * Check the rules for every adornment.
     * 
     * @param The
     *            Alpha Card, whose adornment has been changed.
     * @param adornmentList
     *            The new adornment.
     * 
     * @return True if no rule is violated, otherwise false.
     */
    public static boolean applyRules(final AlphaCard aCard, final Adornment newAdornment) {

        AdornmentType type = AdornmentType.fromName(newAdornment.getName());
        if (type != null && !type.validate(newAdornment.getValue())) {
            return false;
        }

        Adornment oldAdornment = aCard.getAlphaCardDescriptor().getAdornment(newAdornment.getName());

        Adornment deleteAdornment = aCard.getAlphaCardDescriptor().getAdornment(AdornmentType.Deleted.getName());

        if (deleteAdornment != null) {
            // check the adornment Deleted
            if (deleteAdornment.getValue().equals(AdornmentTypeDeleted.TRUE.value()) && type != null
                    && type != AdornmentType.Deleted) {
                return false;
            } else if (deleteAdornment.getValue().equals(AdornmentTypeDeleted.TRUE.value()) && type == null) {
                return false;
            }
        }

        boolean valid = true;

        // check the adornment Validity
        if (type == AdornmentType.Validity) {
            return checkValidityRules(newAdornment, oldAdornment);
        }

        // check the adornment Visibility
        if (type == AdornmentType.Visibility) {
            return checkVisibilityRules(newAdornment, oldAdornment);
        }

        // Payload sequenceNumber must not be editable
        if (newAdornment.getName().equals(AdornmentType.PayloadVersionNumber.getName())) {
            return false;
        }

        return valid;
    }

    /**
     * Gets the new status of Data Provision. Should be called after adornment change.
     * 
     * @param aCard
     *            The Alpha Card whose adornment has been changed.
     * 
     * @return The new status of Data Provision for this Alpha Card
     */
    public static String getDataProvisionStatus(final AlphaCard aCard) {

        Adornment validity = aCard.getAlphaCardDescriptor().getAdornment(AdornmentType.Validity.getName());
        Adornment visibility = aCard.getAlphaCardDescriptor().getAdornment(AdornmentType.Visibility.getName());
        Payload payload = aCard.getPayload();

        // check the rules for Data Provision
        if (payload != null) {
            if (validity != null && validity.getValue().equals(AdornmentTypeValidity.VALID.value())
                    && visibility != null && visibility.getValue().equals(AdornmentTypeVisibility.PUBLIC.value())) {
                return AdornmentTypeDataProvision.FULLFILLED.value();
            }
            return AdornmentTypeDataProvision.INPROGRESS.value();

        }

        return AdornmentTypeDataProvision.OPEN.value();
    }

    /**
     * Check the rules for Validity.
     * 
     * @param newAd
     *            The new adornment.
     * 
     * @param oldAd
     *            The old adornment.
     * 
     * @return True if no rule is violated, otherwise false.
     */
    static boolean checkValidityRules(final Adornment newAd, final Adornment oldAd) {
        return !(newAd.getValue().equals(AdornmentTypeValidity.INVALID.value()) && oldAd.getValue().equals(
                AdornmentTypeValidity.VALID.value()));
    }

    /**
     * Check the rules for Visibility.
     * 
     * @param newAd
     *            The new adornment.
     * 
     * @param oldAd
     *            The old adornment.
     * 
     * @return True if no rule is violated, otherwise false.
     */
    static boolean checkVisibilityRules(final Adornment newAd, final Adornment oldAd) {
        return !(newAd.getValue().equals(AdornmentTypeVisibility.PRIVATE.value()) && oldAd.getValue().equals(
                AdornmentTypeVisibility.PUBLIC.value()));
    }

}
