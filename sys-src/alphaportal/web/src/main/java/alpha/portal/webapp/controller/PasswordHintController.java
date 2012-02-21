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

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.appfuse.model.User;
import org.appfuse.service.MailEngine;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.support.MessageSourceAccessor;
import org.springframework.mail.MailException;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.view.RedirectView;

import alpha.portal.webapp.util.RequestUtil;

/**
 * Simple class to retrieve and send a password hint to users.
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
@Controller
@RequestMapping("/passwordHint*")
public class PasswordHintController {

	/** The log. */
	private final Log log = LogFactory.getLog(PasswordHintController.class);

	/** The user manager. */
	private UserManager userManager = null;

	/** The message source. */
	private MessageSource messageSource = null;

	/** The mail engine. */
	protected MailEngine mailEngine = null;

	/** The message. */
	protected SimpleMailMessage message = null;

	/**
	 * Sets the user manager.
	 * 
	 * @param userManager
	 *            the new user manager
	 */
	@Autowired
	public void setUserManager(final UserManager userManager) {
		this.userManager = userManager;
	}

	/**
	 * Sets the message source.
	 * 
	 * @param messageSource
	 *            the new message source
	 */
	@Autowired
	public void setMessageSource(final MessageSource messageSource) {
		this.messageSource = messageSource;
	}

	/**
	 * Sets the mail engine.
	 * 
	 * @param mailEngine
	 *            the new mail engine
	 */
	@Autowired
	public void setMailEngine(final MailEngine mailEngine) {
		this.mailEngine = mailEngine;
	}

	/**
	 * Sets the message.
	 * 
	 * @param message
	 *            the new message
	 */
	@Autowired
	public void setMessage(final SimpleMailMessage message) {
		this.message = message;
	}

	/**
	 * Handle request.
	 * 
	 * @param request
	 *            the request
	 * @return the model and view
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.GET)
	public ModelAndView handleRequest(final HttpServletRequest request)
			throws Exception {
		this.log.debug("entering 'handleRequest' method...");

		final String username = request.getParameter("username");
		final MessageSourceAccessor text = new MessageSourceAccessor(
				this.messageSource, request.getLocale());

		// ensure that the username has been sent
		if (username == null) {
			this.log.warn("Username not specified, notifying user that it's a required field.");
			request.setAttribute(
					"error",
					text.getMessage("errors.required",
							text.getMessage("user.username")));
			return new ModelAndView("login");
		}

		this.log.debug("Processing Password Hint...");

		// look up the user's information
		try {
			final User user = this.userManager.getUserByUsername(username);

			final StringBuffer msg = new StringBuffer();
			msg.append("Your password hint is: ")
					.append(user.getPasswordHint());
			msg.append("\n\nLogin at: ").append(RequestUtil.getAppURL(request));

			this.message.setTo(user.getEmail());
			final String subject = '[' + text.getMessage("webapp.name") + "] "
					+ text.getMessage("user.passwordHint");
			this.message.setSubject(subject);
			this.message.setText(msg.toString());
			this.mailEngine.send(this.message);

			this.saveMessage(
					request,
					text.getMessage("login.passwordHint.sent", new Object[] {
							username, user.getEmail() }));
		} catch (final UsernameNotFoundException e) {
			this.log.warn(e.getMessage());
			this.saveError(request, text.getMessage("login.passwordHint.error",
					new Object[] { username }));
		} catch (final MailException me) {
			this.log.warn(me.getMessage());
			this.saveError(request, me.getCause().getLocalizedMessage());
		}

		return new ModelAndView(new RedirectView(request.getContextPath()));
	}

	/**
	 * Save error.
	 * 
	 * @param request
	 *            the request
	 * @param error
	 *            the error
	 */
	@SuppressWarnings("unchecked")
	public void saveError(final HttpServletRequest request, final String error) {
		List errors = (List) request.getSession().getAttribute("errors");
		if (errors == null) {
			errors = new ArrayList();
		}
		errors.add(error);
		request.getSession().setAttribute("errors", errors);
	}

	// this method is also in BaseForm Controller
	/**
	 * Save message.
	 * 
	 * @param request
	 *            the request
	 * @param msg
	 *            the msg
	 */
	@SuppressWarnings("unchecked")
	public void saveMessage(final HttpServletRequest request, final String msg) {
		List messages = (List) request.getSession().getAttribute(
				BaseFormController.MESSAGES_KEY);
		if (messages == null) {
			messages = new ArrayList();
		}
		messages.add(msg);
		request.getSession().setAttribute(BaseFormController.MESSAGES_KEY,
				messages);
	}
}
