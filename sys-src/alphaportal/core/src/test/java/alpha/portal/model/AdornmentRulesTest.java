package alpha.portal.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Set;

import org.junit.Before;
import org.junit.Test;

public class AdornmentRulesTest {
    String testCaseId = "myTestCaseId";
    String testCardId = "myTestCardId";
    AlphaCardDescriptor testDescriptor;
    AlphaCard testAlphaCard;
    Adornment newAd;
    Adornment oldAd;
    Set<Adornment> adornmentList;
    Payload payload;

    @Before
    public void setUp() {
        testDescriptor = new AlphaCardDescriptor(testCaseId, testCardId);
        testDescriptor.setAdornment("1", AdornmentTypeVisibility.PRIVATE.value());
        testDescriptor.setAdornment("2", AdornmentTypeValidity.INVALID.value());
        testDescriptor.setAdornment("3", AdornmentTypeDeleted.FALSE.value());
        testAlphaCard = new AlphaCard(testDescriptor);
        adornmentList = testAlphaCard.getAlphaCardDescriptor().getAllAdornments();
        payload = new Payload();
    }

    @Test
    public void testBasics() {
        Adornment newAdornment = new Adornment("gibtsnicht");
        assertTrue(AdornmentRules.applyRules(testAlphaCard, newAdornment));

        newAdornment = new Adornment(AdornmentType.Visibility.getName());
        newAdornment.setValue("falscherwert");
        assertFalse(AdornmentRules.applyRules(testAlphaCard, newAdornment));
    }

    @Test
    public void testApplyRulesVisibility() {
        newAd = new Adornment(AdornmentType.Visibility.getName());
        newAd.setValue(AdornmentTypeVisibility.PRIVATE.value());
        testAlphaCard.getAlphaCardDescriptor().setAdornment(newAd);
        newAd = null;

        newAd = new Adornment(AdornmentType.Visibility.getName());
        newAd.setValue(AdornmentTypeVisibility.PRIVATE.value());
        assertTrue(AdornmentRules.applyRules(testAlphaCard, newAd));

        newAd.setValue(AdornmentTypeVisibility.PUBLIC.value());
        assertTrue(AdornmentRules.applyRules(testAlphaCard, newAd));
        testAlphaCard.getAlphaCardDescriptor().setAdornment(newAd);

        newAd.setValue(AdornmentTypeVisibility.PRIVATE.value());
        assertFalse(AdornmentRules.applyRules(testAlphaCard, newAd));

    }

    @Test
    public void testApplyRulesValidity() {
        newAd = new Adornment(AdornmentType.Validity.getName());
        newAd.setValue(AdornmentTypeValidity.INVALID.value());
        testAlphaCard.getAlphaCardDescriptor().setAdornment(newAd);
        newAd = null;

        newAd = new Adornment(AdornmentType.Validity.getName());
        newAd.setValue(AdornmentTypeValidity.INVALID.value());
        assertTrue(AdornmentRules.applyRules(testAlphaCard, newAd));

        newAd.setValue(AdornmentTypeValidity.VALID.value());
        assertTrue(AdornmentRules.applyRules(testAlphaCard, newAd));
        testAlphaCard.getAlphaCardDescriptor().setAdornment(newAd);

        newAd.setValue(AdornmentTypeValidity.INVALID.value());
        assertFalse(AdornmentRules.applyRules(testAlphaCard, newAd));
    }

    @Test
    public void testDataProvisionStatus() {
        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Validity.name(),
                AdornmentTypeValidity.INVALID.name());
        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Visibility.name(),
                AdornmentTypeVisibility.PRIVATE.name());
        assertEquals(AdornmentTypeDataProvision.OPEN.value(), AdornmentRules.getDataProvisionStatus(testAlphaCard));

        testAlphaCard.setPayload(new Payload());
        assertEquals(AdornmentTypeDataProvision.INPROGRESS.value(), AdornmentRules
                .getDataProvisionStatus(testAlphaCard));

        Payload dummyPayload = new Payload("test", "test/mime");
        dummyPayload.setContent("some test data".getBytes());
        testAlphaCard.setPayload(dummyPayload);
        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Validity.getName(),
                AdornmentTypeValidity.VALID.value());
        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Visibility.getName(),
                AdornmentTypeVisibility.PUBLIC.value());
        assertEquals(AdornmentTypeDataProvision.FULLFILLED.value(), AdornmentRules
                .getDataProvisionStatus(testAlphaCard));

    }

    @Test
    public void testApplyRulesPayloadVersion() {
        String oldVersion = "3";
        String newVersion = "4";

        oldAd = new Adornment(AdornmentType.PayloadVersionNumber.getName());
        oldAd.setValue(oldVersion);

        testAlphaCard.getAlphaCardDescriptor().setAdornment(oldAd);

        newAd = new Adornment(AdornmentType.PayloadVersionNumber.getName());
        newAd.setValue(newVersion);

        boolean success = AdornmentRules.applyRules(testAlphaCard, newAd);

        assertFalse(success);
    }

    @Test
    public void testApplyRulesDeleted() {
        oldAd = new Adornment(AdornmentType.Visibility.getName());
        oldAd.setValue(AdornmentTypeVisibility.PUBLIC.value());

        testAlphaCard.getAlphaCardDescriptor().setAdornment(oldAd);

        newAd = new Adornment(AdornmentType.Deleted.getName());
        newAd.setValue(AdornmentTypeDeleted.FALSE.value());
        testAlphaCard.getAlphaCardDescriptor().setAdornment(newAd);
        assertTrue(AdornmentRules.applyRules(testAlphaCard, newAd));

        newAd.setValue(AdornmentTypeDeleted.TRUE.value());
        testAlphaCard.getAlphaCardDescriptor().setAdornment(newAd);
        assertFalse(AdornmentRules.applyRules(testAlphaCard, oldAd));

        Adornment newAdornment = new Adornment("gibtsnicht");
        testAlphaCard.getAlphaCardDescriptor().setAdornment(newAdornment);
        assertFalse(AdornmentRules.applyRules(testAlphaCard, newAdornment));

        newAd = new Adornment(AdornmentType.Deleted.getName());
        newAd.setValue(AdornmentTypeDeleted.TRUE.value());
        testAlphaCard.getAlphaCardDescriptor().setAdornment(newAd);
        assertTrue(AdornmentRules.applyRules(testAlphaCard, newAd));

        // newAd.setValue(AdornmentTypeDeleted.FALSE.value());
        // assertFalse(AdornmentRules.applyRules(testAlphaCard, newAd));
    }

    @Test
    public void testApplyRulesWithWrongValues() {
        Adornment userAd = new Adornment();
        userAd.setName("myAdornment");

        boolean success = AdornmentRules.applyRules(testAlphaCard, userAd);
        assertTrue(success);

        Adornment validity = new Adornment(AdornmentType.Visibility.getName());
        validity.setValue("wrongvalue");

        success = AdornmentRules.applyRules(testAlphaCard, validity);
        assertFalse(success);
    }

    @Test
    public void testProvisionalData() {
        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Visibility.getName(),
                AdornmentTypeVisibility.PRIVATE.value());
        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Validity.getName(),
                AdornmentTypeValidity.INVALID.value());
        // no payload
        assertTrue(AdornmentRules.getDataProvisionStatus(testAlphaCard) == AdornmentTypeDataProvision.OPEN.value());

        // payload uploaded
        testAlphaCard.setPayload(payload);
        assertTrue(AdornmentRules.getDataProvisionStatus(testAlphaCard) == AdornmentTypeDataProvision.INPROGRESS
                .value());

        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Visibility.getName(),
                AdornmentTypeVisibility.PUBLIC.value());
        assertTrue(AdornmentRules.getDataProvisionStatus(testAlphaCard) == AdornmentTypeDataProvision.INPROGRESS
                .value());

        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Visibility.getName(),
                AdornmentTypeVisibility.PRIVATE.value());
        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Validity.getName(),
                AdornmentTypeValidity.VALID.value());
        assertTrue(AdornmentRules.getDataProvisionStatus(testAlphaCard) == AdornmentTypeDataProvision.INPROGRESS
                .value());

        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Visibility.getName(),
                AdornmentTypeVisibility.PUBLIC.value());
        testAlphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Validity.getName(),
                AdornmentTypeValidity.VALID.value());
        assertTrue(AdornmentRules.getDataProvisionStatus(testAlphaCard) == AdornmentTypeDataProvision.FULLFILLED
                .value());
    }
}
