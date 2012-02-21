/**************************************************************************
 * alpha-Portal: A web portal, for managing knowledge-driven 
 * ad-hoc processes, in form of case files.
 * ==============================================
 * Copyright (C) 2011-2012 by 
 *   - Christoph P. Neumann (http://www.chr15t0ph.de)
 *   - and the SWAT 2011 team
 **************************************************************************
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 **************************************************************************
 * $Id$
 *************************************************************************/
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
 * Implementation of <strong>SimpleFormController</strong> that interacts with
 * the {@link UserManager} to retrieve/persist values to the database.
 * 
 * <p>
 * <a href="UserFormController.java.html"><i>View Source</i></a>
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
@Controller
@RequestMapping("/userform*")
public class UserFormController extends BaseFormController {

	/** The role manager. */
	@Autowired
	private RoleManager roleManager;

	/** The contributor role manager. */
	@Autowired
	private ContributorRoleManager contributorRoleManager;

	/** The user extension manager. */
	@Autowired
	private UserExtensionManager userExtensionManager;

	/**
	 * Instantiates a new user form controller.
	 */
	public UserFormController() {
		this.setCancelView("redirect:/mainMenu");
		this.setSuccessView("redirect:/admin/users");
	}

	/**
	 * On submit.
	 * 
	 * @param user
	 *            the user
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @param model
	 *            the model
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST)
	public String onSubmit(final User user, final BindingResult errors,
			final HttpServletRequest request,
			final HttpServletResponse response, final Model model)
			throws Exception {
		if (request.getParameter("cancel") != null) {
			if (!StringUtils.equals(request.getParameter("from"), "list"))
				return this.getCancelView();
			else
				return this.getSuccessView();
		}

		if (this.validator != null) { // validator is null during testing
			this.validator.validate(user, errors);

			if (errors.hasErrors() && (request.getParameter("delete") == null)) {
				model.addAttribute("contributorRoles",
						this.contributorRoleManager.getAll());
				return "userform";
			}
		}

		this.log.debug("entering 'onSubmit' method...");

		final Locale locale = request.getLocale();

		if (request.getParameter("delete") != null) {
			this.getUserManager().removeUser(user.getId().toString());
			this.saveMessage(request,
					this.getText("user.deleted", user.getFullName(), locale));

			return this.getSuccessView();
		} else {

			// only attempt to change roles if user is admin for other users,
			// showForm() method will handle populating
			if (request.isUserInRole(Constants.ADMIN_ROLE)) {
				final String[] userRoles = request
						.getParameterValues("userRoles");

				if (userRoles != null) {
					user.getRoles().clear();
					for (final String roleName : userRoles) {
						user.addRole(this.roleManager.getRole(roleName));
					}
				}
			}

			final Integer originalVersion = user.getVersion();

			try {
				this.getUserManager().saveUser(user);
			} catch (final AccessDeniedException ade) {
				// thrown by UserSecurityAdvice configured in aop:advisor
				// userManagerSecurity
				this.log.warn(ade.getMessage());
				response.sendError(HttpServletResponse.SC_FORBIDDEN);
				return null;
			} catch (final UserExistsException e) {
				errors.rejectValue("username", "errors.existing.user",
						new Object[] { user.getUsername(), user.getEmail() },
						"duplicate user");

				// redisplay the unencrypted passwords
				user.setPassword(user.getConfirmPassword());
				// reset the version # to what was passed in
				user.setVersion(originalVersion);

				model.addAttribute("contributorRoles",
						this.contributorRoleManager.getAll());
				return "userform";
			}

			if (!StringUtils.equals(request.getParameter("from"), "list")) {
				this.saveMessage(request,
						this.getText("user.saved", user.getFullName(), locale));

				// return to main Menu
				return this.getCancelView();
			} else {
				if (StringUtils.isBlank(request.getParameter("version"))) {
					this.saveMessage(request, this.getText("user.added",
							user.getFullName(), locale));

					// Send an account information e-mail
					this.message.setSubject(this.getText(
							"signup.email.subject", locale));

					try {
						this.sendUserMessage(
								user,
								this.getText("newuser.email.message",
										user.getFullName(), locale),
								RequestUtil.getAppURL(request));
					} catch (final MailException me) {
						this.saveError(request, me.getCause()
								.getLocalizedMessage());
					}

					return this.getSuccessView();
				} else {
					this.saveMessage(
							request,
							this.getText("user.updated.byAdmin",
									user.getFullName(), locale));
				}
			}
		}

		return "redirect:/mainMenu";
	}

	/**
	 * On user role save.
	 * 
	 * @param jspUserExtension
	 *            the jsp user extension
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return the string
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "saveRoles" })
	public String onUserRoleSave(final UserExtension jspUserExtension,
			final HttpServletRequest request, final HttpServletResponse response) {
		// parameters when editing users as an admin
		final String id = request.getParameter("id");
		final String from = request.getParameter("from");
		Long idL = null;
		if (id != null) {
			try {
				idL = Long.parseLong(id);
			} catch (final NumberFormatException e) {
				return "redirect:/caseMenu";
			}
		}
		// get current user for checks
		final User user = this.getUserManager().getUserByUsername(
				request.getRemoteUser());
		// check whether id exists and user is allowed to edit this one
		if (((id != null) && !this.getUserManager().exists(idL))
				|| ((id != null) && (idL != user.getId()) && !request
						.isUserInRole(Constants.ADMIN_ROLE)))
			return "redirect:/caseMenu";
		// reload UserExtension from database or create a new one
		UserExtension userExtension;
		// if user is editing himself
		if (id == null) {
			if (this.userExtensionManager.exists(user.getId())) {
				userExtension = this.userExtensionManager.get(user.getId());
			} else {
				userExtension = new UserExtension(this.getUserManager().get(
						user.getId()));
			}
		} else {// if admin is editing other users
			if (this.userExtensionManager.exists(idL)) {
				userExtension = this.userExtensionManager.get(idL);
			} else {
				userExtension = new UserExtension(this.getUserManager()
						.get(idL));
			}
		}
		// parse submitted roleIds to ContributorRole
		final Set<ContributorRole> roles = new HashSet<ContributorRole>();
		if ((jspUserExtension != null) && (jspUserExtension.getRoles() != null)) {
			for (final ContributorRole jspRole : jspUserExtension.getRoles()) {
				Long roleId = null;
				try {
					// WTF, Spring puts the submitted roleIds into name!
					roleId = Long.parseLong(jspRole.getName());
				} catch (final NumberFormatException e) {
					this.saveError(request, "userextension.invalidRole");
					continue;
				}
				if (!this.contributorRoleManager.exists(roleId)) {
					this.saveError(request, "userextension.invalidRole");
					continue;
				} else {
					roles.add(this.contributorRoleManager.get(roleId));
				}
			}
		}
		userExtension.setRoles(roles);
		userExtension = this.userExtensionManager.save(userExtension);

		return "redirect:/userform?id=" + userExtension.getUserId()
				+ (from != null ? "&from=" + from : "");

	}

	/**
	 * Show form.
	 * 
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return the model and view
	 * @throws Exception
	 *             the exception
	 */
	@ModelAttribute
	@RequestMapping(method = { RequestMethod.GET, RequestMethod.POST })
	protected ModelAndView showForm(final HttpServletRequest request,
			final HttpServletResponse response) throws Exception {

		final ModelAndView model = new ModelAndView();
		User user;

		// If not an administrator, make sure user is not trying to add or edit
		// another user
		if (!request.isUserInRole(Constants.ADMIN_ROLE)
				&& !this.isFormSubmission(request)) {
			if (this.isAdd(request) || (request.getParameter("id") != null)) {
				response.sendError(HttpServletResponse.SC_FORBIDDEN);
				this.log.warn("User '" + request.getRemoteUser()
						+ "' is trying to edit user with id '"
						+ request.getParameter("id") + "'");

				throw new AccessDeniedException(
						"You do not have permission to modify other users.");
			}
		}

		if (!this.isFormSubmission(request)) {
			final String userId = request.getParameter("id");

			// if user logged in with remember me, display a warning that they
			// can't change passwords
			this.log.debug("checking for remember me login...");

			final AuthenticationTrustResolver resolver = new AuthenticationTrustResolverImpl();
			final SecurityContext ctx = SecurityContextHolder.getContext();

			if (ctx.getAuthentication() != null) {
				final Authentication auth = ctx.getAuthentication();

				if (resolver.isRememberMe(auth)) {
					request.getSession().setAttribute("cookieLogin", "true");

					// add warning message
					this.saveMessage(
							request,
							this.getText("userProfile.cookieLogin",
									request.getLocale()));
				}
			}

			if ((userId == null) && !this.isAdd(request)) {
				user = this.getUserManager().getUserByUsername(
						request.getRemoteUser());
			} else if (!StringUtils.isBlank(userId)
					&& !"".equals(request.getParameter("version"))) {
				user = this.getUserManager().getUser(userId);
			} else {
				user = new User();
				user.addRole(new Role(Constants.USER_ROLE));
			}

			user.setConfirmPassword(user.getPassword());

			UserExtension userExtension;
			final Long uId = user.getId();
			if ((uId != null) && this.userExtensionManager.exists(uId)) {
				userExtension = this.userExtensionManager.get(uId);
			} else {
				userExtension = new UserExtension(user);
			}

			model.addObject("userExtension", userExtension);
			model.addObject("contributorRoles",
					this.contributorRoleManager.getAll());

		} else {
			// populate user object from database, so all fields don't need to
			// be hidden fields in form
			user = this.getUserManager().getUser(request.getParameter("id"));
		}

		model.addObject("user", user);

		return model;
	}

	/**
	 * Checks if is form submission.
	 * 
	 * @param request
	 *            the request
	 * @return true, if is form submission
	 */
	private boolean isFormSubmission(final HttpServletRequest request) {
		return request.getMethod().equalsIgnoreCase("post");
	}

	/**
	 * Checks if is adds the.
	 * 
	 * @param request
	 *            the request
	 * @return true, if is adds the
	 */
	protected boolean isAdd(final HttpServletRequest request) {
		final String method = request.getParameter("method");
		return ((method != null) && method.equalsIgnoreCase("add"));
	}
}
