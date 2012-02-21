package alpha.portal.webapp.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.AlphaCase;
import alpha.portal.service.CaseManager;

/**
 * 
 */

public class CaseMenuControllerTest extends BaseControllerTestCase {

    @Autowired
    private CaseMenuController ctrl;

    @Autowired
    private UserManager userManager;

    @Autowired
    private CaseManager caseManager;

    @SuppressWarnings("unchecked")
    @Test
    public void testHandleRequest() throws Exception {
        MockHttpServletRequest request = newGet("/caseMenu");

        User u = new User("ichbineintollertesterdenesnochnichtgibt");
        u.setEmail("l@m.d");
        u.setFirstName("l");
        u.setLastName("m");
        u.setPassword("123");
        u = userManager.save(u);

        AlphaCase c = new AlphaCase();
        c.setName("blablabla");
        c.addParticipant(u);
        c = caseManager.save(c);

        request.setRemoteUser("ichbineintollertesterdenesnochnichtgibt");

        ModelAndView result = ctrl.handleRequest(request);

        List<AlphaCase> lCases = (List<AlphaCase>) result.getModel().get("caseList");
        assertEquals(1, lCases.size());
        AlphaCase c2 = lCases.get(0);
        assertEquals(c, c2);

        caseManager.remove(c.getCaseId());
        result = ctrl.handleRequest(request);
        lCases = (List<AlphaCase>) result.getModel().get("caseList");
        assertTrue(lCases == null || lCases.isEmpty());

        userManager.remove(u.getId());
    }

}
