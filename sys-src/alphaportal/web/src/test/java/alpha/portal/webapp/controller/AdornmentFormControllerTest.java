package alpha.portal.webapp.controller;

import org.appfuse.service.GenericManager;
import org.aspectj.lang.annotation.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.Adornment;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.ContributorRoleManager;
import alpha.portal.service.UserExtensionManager;

public class AdornmentFormControllerTest {

    private static final String CASE_ID = "550e4713-e22b-11d4-a716-446655440002";
    private static final String CARD_ID = "440e4816-e01b-74d4-a716-449955440097";

    @Autowired
    private AlphaCardManager alphaCardManager;

    @Autowired
    private ContributorRoleManager contributorRoleManager;

    @Autowired
    private GenericManager<Adornment, Long> adornmentManager;

    @Autowired
    private UserExtensionManager userExtensionManager;

    private AlphaCard alphaCard;

    private Adornment adornment;

    @Before(value = "")
    public void setUp() {
        AlphaCardIdentifier identifier = new AlphaCardIdentifier(CASE_ID, CARD_ID);
        alphaCard = new AlphaCard();
        alphaCard.setAlphaCardIdentifier(identifier);
    }

    @Test
    public void testSetupAdornmentTypes() {

    }

    @Test
    public void testShowForm() {

    }

    @Test
    public void onSubmitForm() {

    }
}
