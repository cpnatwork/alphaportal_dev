package alpha.portal.webapp.util;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeDataProvision;
import alpha.portal.model.AdornmentTypeDeleted;
import alpha.portal.model.AlphaCard;

/**
 * This class implements some function which can be used from jsp-Pages by the JSP Expression language
 * 
 * @author uz87odun
 * 
 */

public class Functions {

    /**
     * Get an adornment's value of the given card by the adornment's name
     * 
     * @param adornmentName
     *            the name of the adornment
     * @param card
     *            the card to use
     * @return the value of the adornment or an empty String if none found
     */
    public static String adornmentValueFromCard(final String adornmentName, final AlphaCard card) {
        if (card != null && card.getAlphaCardDescriptor() != null) {
            Adornment adornment = card.getAlphaCardDescriptor().getAdornment(adornmentName);
            if (adornment != null && adornment.getValue() != null)
                return adornment.getValue();
        }
        return "";
    }

    /**
     * Get the data-provision-sate from card
     * 
     * @param card
     *            the card to use
     * @return the data-provision state of this card
     */
    public static String dataProvisionFromCard(final AlphaCard card) {
        String adornmentValue = adornmentValueFromCard(AdornmentType.DataProvision.getName(), card);
        if (!StringUtils.isBlank(adornmentValue)) {
            AdornmentTypeDataProvision dataProvision = AdornmentTypeDataProvision.fromValue(adornmentValue);
            if (dataProvision != null)
                return dataProvision.name().toLowerCase();
        }
        return "";
    }

    public static boolean isCardMarkedAsDeleted(final AlphaCard card) {
        String adornmentValue = adornmentValueFromCard(AdornmentType.Deleted.getName(), card);
        if (!StringUtils.isBlank(adornmentValue)) {
            AdornmentTypeDeleted adornmentType = AdornmentTypeDeleted.fromValue(adornmentValue);
            return adornmentType.equals(AdornmentTypeDeleted.TRUE);
        }
        return false;
    }

    public static boolean isUserContributor(final Long userId, final AlphaCard card) {
        if (card != null && card.getAlphaCardDescriptor() != null) {
            Long contributor = card.getAlphaCardDescriptor().getContributor();
            if (contributor != null) {
                return contributor.equals(userId);
            }
        }
        return false;
    }

    @SuppressWarnings("unchecked")
    public static boolean contains(final String[] list, final String o) {
        return ArrayUtils.contains(list, o);
    }
}
