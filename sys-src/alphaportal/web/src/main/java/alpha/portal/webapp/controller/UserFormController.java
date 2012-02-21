package alpha.portal.webapp.controller;

import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.appfuse.Constants;
import org.appfuse.model.Role;
import org.appfuse.model.User;
import org.appfuse.service.RoleManager;
import org.appfuse.service.UserExistsException;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.MailException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.AuthenticationTrustResolver;
import org.springframework.security.authentication.AuthenticationTrustResolverImpl;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.service.ContributorRoleManager;
import alpha.portal.service.UserExtensionManager;
import alpha.portal.webapp.util.RequestUtil;

/**
 * Implementation of <strong>SimpleFormController</strong> that interacts with the {@link UserManager} to
 * retrieve/persist values to the database.
 * 
 * <p>
 * <a href="UserFormController.java.html"><i>View Source</i></a>
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
@Controller
@RequestMapping("/userform*")
public class UserFormController extends BaseFormController {

    @Autowired
    private RoleManager roleManager;

    @Autowired
    private ContributorRoleManager contributorRoleManager;

    @Autowired
    private UserExtensionManager userExtensionManager;

    public UserFormController() {
        setCancelView("redirect:/mainMenu");
        setSuccessView("redirect:/admin/users");
    }

    @RequestMapping(method = RequestMethod.POST)
    public String onSubmit(final User user, final BindingResult errors, final HttpServletRequest request,
            final HttpServletResponse response, final Model model) throws Exception {
        if (request.getParameter("cancel") != null) {
            if (!StringUtils.equals(request.getParameter("from"), "list"))
                return getCancelView();
            else
                return getSuccessView();
        }

        if (validator != null) { // validator is null during testing
            validator.validate(user, errors);

            if (errors.hasErrors() && request.getParameter("delete") == null) {
                model.addAttribute("contributorRoles", contributorRoleManager.getAll());
                return "userform";
            }
        }

        log.debug("entering 'onSubmit' method...");

        Locale locale = request.getLocale();

        if (request.getParameter("delete") != null) {
            getUserManager().removeUser(user.getId().toString());
            saveMessage(request, getText("user.deleted", user.getFullName(), locale));

            return getSuccessView();
        } else {

            // only attempt to change roles if user is admin for other users,
            // showForm() method will handle populating
            if (request.isUserInRole(Constants.ADMIN_ROLE)) {
                String[] userRoles = request.getParameterValues("userRoles");

                if (userRoles != null) {
                    user.getRoles().clear();
                    for (String roleName : userRoles) {
                        user.addRole(roleManager.getRole(roleName));
                    }
                }
            }

            Integer originalVersion = user.getVersion();

            try {
                getUserManager().saveUser(user);
            } catch (AccessDeniedException ade) {
                // thrown by UserSecurityAdvice configured in aop:advisor userManagerSecurity
                log.warn(ade.getMessage());
                response.sendError(HttpServletResponse.SC_FORBIDDEN);
                return null;
            } catch (UserExistsException e) {
                errors.rejectValue("username", "errors.existing.user", new Object[] { user.getUsername(),
                        user.getEmail() }, "duplicate user");

                // redisplay the unencrypted passwords
                user.setPassword(user.getConfirmPassword());
                // reset the version # to what was passed in
                user.setVersion(originalVersion);

                model.addAttribute("contributorRoles", contributorRoleManager.getAll());
                return "userform";
            }

            if (!StringUtils.equals(request.getParameter("from"), "list")) {
                saveMessage(request, getText("user.saved", user.getFullName(), locale));

                // return to main Menu
                return getCancelView();
            } else {
                if (StringUtils.isBlank(request.getParameter("version"))) {
                    saveMessage(request, getText("user.added", user.getFullName(), locale));

                    // Send an account information e-mail
                    message.setSubject(getText("signup.email.subject", locale));

                    try {
                        sendUserMessage(user, getText("newuser.email.message", user.getFullName(), locale), RequestUtil
                                .getAppURL(request));
                    } catch (MailException me) {
                        saveError(request, me.getCause().getLocalizedMessage());
                    }

                    return getSuccessView();
                } else {
                    saveMessage(request, getText("user.updated.byAdmin", user.getFullName(), locale));
                }
            }
        }

        return "redirect:/mainMenu";
    }

    @RequestMapping(method = RequestMethod.POST, params = { "saveRoles" })
    public String onUserRoleSave(final UserExtension jspUserExtension, final HttpServletRequest request,
            final HttpServletResponse response) {
        // parameters when editing users as an admin
        String id = request.getParameter("id");
        String from = request.getParameter("from");
        Long idL = null;
        if (id != null) {
            try {
                idL = Long.parseLong(id);
            } catch (NumberFormatException e) {
                return "redirect:/caseMenu";
            }
        }
        // get current user for checks
        User user = getUserManager().getUserByUsername(request.getRemoteUser());
        // check whether id exists and user is allowed to edit this one
        if ((id != null && !getUserManager().exists(idL))
                || (id != null && idL != user.getId() && !request.isUserInRole(Constants.ADMIN_ROLE)))
            return "redirect:/caseMenu";
        // reload UserExtension from database or create a new one
        UserExtension userExtension;
        // if user is editing himself
        if (id == null) {
            if (userExtensionManager.exists(user.getId())) {
                userExtension = userExtensionManager.get(user.getId());
            } else {
                userExtension = new UserExtension(getUserManager().get(user.getId()));
            }
        } else {// if admin is editing other users
            if (userExtensionManager.exists(idL)) {
                userExtension = userExtensionManager.get(idL);
            } else {
                userExtension = new UserExtension(getUserManager().get(idL));
            }
        }
        // parse submitted roleIds to ContributorRole
        Set<ContributorRole> roles = new HashSet<ContributorRole>();
        if (jspUserExtension != null && jspUserExtension.getRoles() != null) {
            for (ContributorRole jspRole : jspUserExtension.getRoles()) {
                Long roleId = null;
                try {
                    // WTF, Spring puts the submitted roleIds into name!
                    roleId = Long.parseLong(jspRole.getName());
                } catch (NumberFormatException e) {
                    saveError(request, "userextension.invalidRole");
                    continue;
                }
                if (!contributorRoleManager.exists(roleId)) {
                    saveError(request, "userextension.invalidRole");
                    continue;
                } else {
                    roles.add(contributorRoleManager.get(roleId));
                }
            }
        }
        userExtension.setRoles(roles);
        userExtension = userExtensionManager.save(userExtension);

        return "redirect:/userform?id=" + userExtension.getUserId() + (from != null ? "&from=" + from : "");

    }

    @ModelAttribute
    @RequestMapping(method = { RequestMethod.GET, RequestMethod.POST })
    protected ModelAndView showForm(final HttpServletRequest request, final HttpServletResponse response)
            throws Exception {

        ModelAndView model = new ModelAndView();
        User user;

        // If not an administrator, make sure user is not trying to add or edit another user
        if (!request.isUserInRole(Constants.ADMIN_ROLE) && !isFormSubmission(request)) {
            if (isAdd(request) || request.getParameter("id") != null) {
                response.sendError(HttpServletResponse.SC_FORBIDDEN);
                log.warn("User '" + request.getRemoteUser() + "' is trying to edit user with id '"
                        + request.getParameter("id") + "'");

                throw new AccessDeniedException("You do not have permission to modify other users.");
            }
        }

        if (!isFormSubmission(request)) {
            String userId = request.getParameter("id");

            // if user logged in with remember me, display a warning that they can't change passwords
            log.debug("checking for remember me login...");

            AuthenticationTrustResolver resolver = new AuthenticationTrustResolverImpl();
            SecurityContext ctx = SecurityContextHolder.getContext();

            if (ctx.getAuthentication() != null) {
                Authentication auth = ctx.getAuthentication();

                if (resolver.isRememberMe(auth)) {
                    request.getSession().setAttribute("cookieLogin", "true");

                    // add warning message
                    saveMessage(request, getText("userProfile.cookieLogin", request.getLocale()));
                }
            }

            if (userId == null && !isAdd(request)) {
                user = getUserManager().getUserByUsername(request.getRemoteUser());
            } else if (!StringUtils.isBlank(userId) && !"".equals(request.getParameter("version"))) {
                user = getUserManager().getUser(userId);
            } else {
                user = new User();
                user.addRole(new Role(Constants.USER_ROLE));
            }

            user.setConfirmPassword(user.getPassword());

            UserExtension userExtension;
            Long uId = user.getId();
            if (uId != null && userExtensionManager.exists(uId)) {
                userExtension = userExtensionManager.get(uId);
            } else {
                userExtension = new UserExtension(user);
            }

            model.addObject("userExtension", userExtension);
            model.addObject("contributorRoles", contributorRoleManager.getAll());

        } else {
            // populate user object from database, so all fields don't need to be hidden fields in form
            user = getUserManager().getUser(request.getParameter("id"));
        }

        model.addObject("user", user);

        return model;
    }

    private boolean isFormSubmission(final HttpServletRequest request) {
        return request.getMethod().equalsIgnoreCase("post");
    }

    protected boolean isAdd(final HttpServletRequest request) {
        String method = request.getParameter("method");
        return (method != null && method.equalsIgnoreCase("add"));
    }
}
