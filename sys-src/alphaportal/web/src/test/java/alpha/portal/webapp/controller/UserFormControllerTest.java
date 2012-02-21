package alpha.portal.webapp.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Map;

import org.appfuse.Constants;
import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.ui.ExtendedModelMap;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;

import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;

public class UserFormControllerTest extends BaseControllerTestCase {

    @Autowired
    private UserFormController c = null;
    private MockHttpServletRequest request;
    private User user;

    @Test
    public void testAdd() throws Exception {
        log.debug("testing add new user...");
        request = newGet("/userform.html");
        request.addParameter("method", "Add");
        request.addUserRole(Constants.ADMIN_ROLE);

        Map<String, Object> model = c.showForm(request, new MockHttpServletResponse()).getModel();
        Object object = model.get("user");
        assertTrue(object instanceof User);
        user = (User) object;
        assertNull(user.getUsername());
    }

    @Test
    public void testAddWithoutPermission() throws Exception {
        log.debug("testing add new user...");
        request = newGet("/userform.html");
        request.addParameter("method", "Add");

        try {
            c.showForm(request, new MockHttpServletResponse());
            fail("AccessDeniedException not thrown...");
        } catch (AccessDeniedException ade) {
            assertNotNull(ade.getMessage());
        }
    }

    @Test
    public void testCancel() throws Exception {
        log.debug("testing cancel...");
        request = newPost("/userform.html");
        request.addParameter("cancel", "");

        BindingResult errors = new DataBinder(user).getBindingResult();
        String view = c.onSubmit(user, errors, request, new MockHttpServletResponse(), null);

        assertEquals("redirect:/mainMenu", view);
    }

    @Test
    public void testEdit() throws Exception {
        log.debug("testing edit...");
        request = newGet("/userform.html");
        request.addParameter("id", "-1");
        request.addUserRole(Constants.ADMIN_ROLE);

        Map<String, Object> model = c.showForm(request, new MockHttpServletResponse()).getModel();
        Object object = model.get("user");
        assertTrue(object instanceof User);
        user = (User) object;
        assertEquals("Tomcat User", user.getFullName());
    }

    @Test
    public void testEditWithoutPermission() throws Exception {
        log.debug("testing edit...");
        request = newGet("/userform.html");
        request.addParameter("id", "-1"); // regular user

        try {
            c.showForm(request, new MockHttpServletResponse());
            fail("AccessDeniedException not thrown...");
        } catch (AccessDeniedException ade) {
            assertNotNull(ade.getMessage());
        }
    }

    @Test
    public void testEditProfile() throws Exception {
        log.debug("testing edit profile...");
        request = newGet("/userform.html");
        request.setRemoteUser("user");

        Map<String, Object> model = c.showForm(request, new MockHttpServletResponse()).getModel();
        Object object = model.get("user");
        assertTrue(object instanceof User);
        user = (User) object;
        assertEquals("Tomcat User", user.getFullName());
    }

    @Test
    public void testSave() throws Exception {
        request = newPost("/userform.html");
        // set updated properties first since adding them later will
        // result in multiple parameters with the same name getting sent
        User user = ((UserManager) applicationContext.getBean("userManager")).getUser("-1");
        user.setConfirmPassword(user.getPassword());
        user.setLastName("Updated Last Name");

        BindingResult errors = new DataBinder(user).getBindingResult();
        c.onSubmit(user, errors, request, new MockHttpServletResponse(), null);

        assertFalse(errors.hasErrors());
        assertNotNull(request.getSession().getAttribute("successMessages"));
    }

    @Test
    public void testAddWithMissingFields() throws Exception {
        request = newPost("/userform.html");
        user = new User();
        user.setFirstName("Jack");
        request.setRemoteUser("user");

        BindingResult errors = new DataBinder(user).getBindingResult();
        c.onSubmit(user, errors, request, new MockHttpServletResponse(), new ExtendedModelMap());

        assertTrue(errors.getAllErrors().size() == 10);
    }

    @Test
    public void testRemove() throws Exception {
        request = newPost("/userform.html");
        request.addParameter("delete", "");
        user = new User();
        user.setId(-2L);

        BindingResult errors = new DataBinder(user).getBindingResult();
        c.onSubmit(user, errors, request, new MockHttpServletResponse(), null);

        assertNotNull(request.getSession().getAttribute("successMessages"));
    }

    @Test
    public void testContibutorRoleStuff() throws Exception {
        request = newGet("/userform.html");
        request.setRemoteUser("admin");

        Map<String, Object> model = c.showForm(request, new MockHttpServletResponse()).getModel();
        Object object = model.get("userExtension");
        assertNotNull(object);
        assertTrue(object instanceof UserExtension);
        object = model.get("contributorRoles");
        assertNotNull(object);
        assertTrue(object instanceof ArrayList<?>);
        ArrayList<?> objList = (ArrayList<?>) object;
        if (objList.size() > 0) {
            assertTrue(objList.get(0) instanceof ContributorRole);
        }
    }
}
