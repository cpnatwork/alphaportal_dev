package alpha.portal.webapp.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;

import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.AlphaCase;
import alpha.portal.service.CaseManager;
import alpha.portal.webapp.util.CardFilterHolder;

public class CaseFormControllerTest extends BaseControllerTestCase {
    private static final String caseId = "550e4713-e22b-11d4-a716-446655440000";

    @Autowired
    private CaseFormController form;

    @Autowired
    private UserManager userManager;

    @Autowired
    private CaseManager caseManager;

    private CardFilterHolder filters = new CardFilterHolder();

    /**
     * This test is run here since we don't have a userManager available in the core module and participants are
     * connected with a case
     */
    @Test
    public void testCaseCRA() {
        User u = userManager.get(-2L);
        AlphaCase aCase = caseManager.get(caseId);
        assertNotNull(aCase);
        assertNotNull(aCase.getListOfParticipants());
        aCase.addParticipant(u);
        aCase = caseManager.save(aCase);

        Set<User> participants = aCase.getListOfParticipants();
        // this must be true from sample data
        aCase = caseManager.get(caseId);
        assertTrue(participants.contains(u));
    }

    @Test
    public void testNew() throws Exception {
        MockHttpServletRequest request = newGet("/caseform");
        request.setRemoteUser("admin");

        HttpServletResponse response = new MockHttpServletResponse();
        ModelAndView mv = form.showForm(filters, request, response);
        assertNotNull(mv);
        assertEquals("caseform", mv.getViewName());
        assertEquals(new AlphaCase(), mv.getModel().get("case"));
    }

    @Test
    public void testLast() throws Exception {
        MockHttpServletRequest request = newGet("/caseform");
        request.setParameter("caseId", "550e4713-e22b-11d4-a716-446655440002");
        request.setRemoteUser("admin");
        ModelAndView mv = form.showForm(filters, request, new MockHttpServletResponse());

        request = newGet("/caseform");
        request.setParameter("caseId", "last");
        request.setRemoteUser("admin");

        AlphaCase aCase = caseManager.get("550e4713-e22b-11d4-a716-446655440002");
        mv = form.showForm(filters, request, new MockHttpServletResponse());
        assertEquals("caseform", mv.getViewName());
        assertEquals(aCase, mv.getModel().get("case"));
    }

    @Test
    public void testAdd() throws Exception {
        MockHttpServletRequest request = newGet("/caseform");
        request.setRemoteUser("admin");
        ModelAndView mv = form.showForm(filters, request, new MockHttpServletResponse());

        request = newPost("/caseform");
        request.setRemoteUser("admin");
        AlphaCase aCase = (AlphaCase) mv.getModel().get("case");
        aCase.setName("test case which does not exist yet");
        BindingResult errors = new DataBinder(aCase).getBindingResult();
        String view = form.addCase(aCase, errors, request, new MockHttpServletResponse());

        List<AlphaCase> dbCases = caseManager.findByName(aCase.getName());
        assertNotNull(dbCases);
        assertTrue(dbCases.size() >= 1);
        AlphaCase dbCase = dbCases.get(0);
        assertNotNull(dbCase);
        assertEquals("redirect:/caseform?caseId=" + dbCase.getCaseId(), view);
        assertFalse(errors.hasErrors());
        assertNotNull(request.getSession().getAttribute("successMessages"));

        Locale locale = request.getLocale();
        ArrayList<Object> msgs = (ArrayList<Object>) request.getSession().getAttribute("successMessages");
        assertTrue(msgs.contains(form.getText("case.added", locale)));
    }

    @Test
    public void testEdit() throws Exception {
        MockHttpServletRequest request = newGet("/caseform");
        request.setParameter("caseId", caseId);
        request.setRemoteUser("admin");
        ModelAndView mv = form.showForm(filters, request, new MockHttpServletResponse());
        assertEquals("caseform", mv.getViewName());
        AlphaCase aCase = (AlphaCase) mv.getModel().get("case");
        AlphaCase dbCase = caseManager.get(caseId);
        assertEquals(dbCase, aCase);
        assertEquals(dbCase.getAlphaCards(), mv.getModel().get("cards"));
        assertEquals(dbCase.getListOfParticipants(), mv.getModel().get("participants"));

        request = newPost("/caseform");
        request.setRemoteUser("admin");
        aCase.setName("test case with a new name");
        BindingResult errors = new DataBinder(aCase).getBindingResult();
        String view = form.saveCase(aCase, errors, request, new MockHttpServletResponse());
        assertEquals("redirect:/caseform?caseId=" + aCase.getCaseId(), view);
        assertFalse(errors.hasErrors());
        assertNotNull(request.getSession().getAttribute("successMessages"));

        Locale locale = request.getLocale();
        ArrayList<Object> msgs = (ArrayList<Object>) request.getSession().getAttribute("successMessages");
        assertTrue(msgs.contains(form.getText("case.updated", locale)));

        dbCase = caseManager.get(caseId);
        assertEquals(dbCase, aCase);
    }

    @Test
    public void testDelete() throws Exception {
        MockHttpServletRequest request = newGet("/caseform");
        request.setParameter("caseId", caseId);
        request.setRemoteUser("admin");
        ModelAndView mv = form.showForm(filters, request, new MockHttpServletResponse());
        AlphaCase myCase = (AlphaCase) mv.getModel().get("case");

        request = newPost("/caseform");
        request.setRemoteUser("admin");
        request.addParameter("delete", "");

        BindingResult errors = new DataBinder(myCase).getBindingResult();
        String view = form.deleteCase(myCase, errors, request);
        assertEquals(form.getCancelView(), view);
        assertNotNull(request.getSession().getAttribute("successMessages"));

        Locale locale = request.getLocale();
        ArrayList<Object> msgs = (ArrayList<Object>) request.getSession().getAttribute("successMessages");
        assertTrue(msgs.contains(form.getText("case.deleted", locale)));

        assertFalse(caseManager.exists(caseId));
    }
}