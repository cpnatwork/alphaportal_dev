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

import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.appfuse.Constants;
import org.appfuse.model.User;
import org.appfuse.service.RoleManager;
import org.appfuse.service.UserExistsException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.MailException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.webapp.util.RequestUtil;

/**
 * Controller to signup new users.
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
@Controller
@RequestMapping("/signup*")
public class SignupController extends BaseFormController {

	/** The role manager. */
	private RoleManager roleManager;

	/**
	 * Sets the role manager.
	 * 
	 * @param roleManager
	 *            the new role manager
	 */
	@Autowired
	public void setRoleManager(final RoleManager roleManager) {
		this.roleManager = roleManager;
	}

	/**
	 * Instantiates a new signup controller.
	 */
	public SignupController() {
		this.setCancelView("redirect:/login");
		this.setSuccessView("redirect:/mainMenu");
	}

	/**
	 * Show form.
	 * 
	 * @return the user
	 */
	@ModelAttribute
	@RequestMapping(method = RequestMethod.GET)
	public User showForm() {
		return new User();
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
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST)
	public String onSubmit(final User user, final BindingResult errors,
			final HttpServletRequest request, final HttpServletResponse response)
			throws Exception {
		if (request.getParameter("cancel") != null)
			return this.getCancelView();

		if (this.log.isDebugEnabled()) {
			this.log.debug("entering 'onSubmit' method...");
		}
		final Locale locale = request.getLocale();

		user.setEnabled(true);

		// Set the default user role on this new user
		user.addRole(this.roleManager.getRole(Constants.USER_ROLE));

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
			return "signup";
		}

		this.saveMessage(request,
				this.getText("user.registered", user.getUsername(), locale));
		request.getSession().setAttribute(Constants.REGISTERED, Boolean.TRUE);

		// log user in automatically
		final UsernamePasswordAuthenticationToken auth = new UsernamePasswordAuthenticationToken(
				user.getUsername(), user.getConfirmPassword(),
				user.getAuthorities());
		auth.setDetails(user);
		SecurityContextHolder.getContext().setAuthentication(auth);

		// Send user an e-mail
		if (this.log.isDebugEnabled()) {
			this.log.debug("Sending user '" + user.getUsername()
					+ "' an account information e-mail");
		}

		// Send an account information e-mail
		this.message.setSubject(this.getText("signup.email.subject", locale));

		try {
			this.sendUserMessage(user,
					this.getText("signup.email.message", locale),
					RequestUtil.getAppURL(request));
		} catch (final MailException me) {
			this.saveError(request, me.getMostSpecificCause().getMessage());
		}

		return this.getSuccessView();
	}
}
