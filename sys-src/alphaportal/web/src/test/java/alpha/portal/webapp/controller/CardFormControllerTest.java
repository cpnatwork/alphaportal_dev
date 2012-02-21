package alpha.portal.webapp.controller;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.service.AlphaCardManager;

public class CardFormControllerTest extends BaseControllerTestCase {

    private static final String CASE_ID = "550e4713-e22b-11d4-a716-446655440000";
    private static final String CARD_ID = "440e4816-e01b-74d4-a716-449955440092";

    @Autowired
    private CardFormController c;

    @Autowired
    private AlphaCardManager alphaCardManager;

    private AlphaCard alphaCard;

    @Before
    public void setUp() {
        AlphaCardIdentifier identifier = new AlphaCardIdentifier(CASE_ID, CARD_ID);
        alphaCard = new AlphaCard();
        alphaCard.setAlphaCardIdentifier(identifier);
    }

    @Test
    public void testShowForm() {
        MockHttpServletRequest request = newGet("/caseform");
        request.addParameter("case", CASE_ID);
        request.addParameter("card", CARD_ID);
        c.showForm(request);
    }

    @Test
    public void testOnSubmit() throws Exception {
        MockHttpServletRequest request = newPost("/cardform");
        String view = c.onSubmit(alphaCard, null, request, new MockHttpServletResponse());
        assertEquals("redirect:/caseform?caseId=" + CASE_ID, view);
    }

    @Test
    public void testSaveCard() throws Exception {
        MockHttpServletRequest request = newPost("/cardform");
        request.setRemoteUser("admin");
        BindingResult errors = new DataBinder(alphaCard).getBindingResult();
        String view = c.saveCard(alphaCard, errors, request, new MockHttpServletResponse());
        assertEquals( "redirect:/caseform?activeCardId=" + CARD_ID + "&caseId=" + CASE_ID, view);

        MockHttpServletRequest request2 = newPost("/cardform");
        request2.setRemoteUser("admin");
        AlphaCardIdentifier identifier = new AlphaCardIdentifier(CASE_ID);
        AlphaCard alphaCard2 = new AlphaCard();
        alphaCard2.setAlphaCardIdentifier(identifier);

        c.saveCard(alphaCard2, null, request2, new MockHttpServletResponse());
        // unable to verify cardId since the controller doesn't return the saved card to us :/
    }

    @Test
    public void testCancelCard() throws Exception {
        MockHttpServletRequest request = newPost("/cardform");
        String view = c.cancelCard(alphaCard, null, request);
        assertEquals("redirect:/caseform?caseId=" + CASE_ID, view);
    }

    @Test
    public void testAssignCard() throws Exception {
        MockHttpServletRequest request = newPost("/cardform");
        request.setRemoteUser("admin");
        String view = c.assignCard(alphaCard, null, request);
        assertEquals("redirect:/caseform?caseId=" + CASE_ID + "&activeCardId=" + CARD_ID, view);
    }

    @Test
    public void testUnassignCard() throws Exception {
        MockHttpServletRequest request = newPost("/cardform");
        String view = c.unassignCard(alphaCard, null, request);
        assertEquals("redirect:/caseform?caseId=" + CASE_ID + "&activeCardId=" + CARD_ID, view);
    }

    @Test
    public void testGetPayload() throws IOException {
        String view = c.getPayload(alphaCard, new MockHttpServletResponse());
        assertEquals("redirect:/caseform?caseId=" + CASE_ID + "&activeCardId=" + CARD_ID, view);
    }

    @Test
    public void testDeletePayload() throws IOException {
        MockHttpServletRequest request = newPost("/cardform");
        String view = c.deletePayload(alphaCard, request);
        assertEquals("redirect:/caseform?caseId=" + CASE_ID + "&activeCardId=" + CARD_ID, view);
    }
}