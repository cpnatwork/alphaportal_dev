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

import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.model.AlphaCase;
import alpha.portal.service.CaseManager;

/**
 * The Class UserSearchController.
 */
@Controller
@RequestMapping("/userSearch*")
public class UserSearchController {

	/** The user manager. */
	@Autowired
	private UserManager userManager;

	/** The case manager. */
	@Autowired
	private CaseManager caseManager;

	/**
	 * Show form.
	 */
	@RequestMapping(method = RequestMethod.GET)
	protected void showForm() {
	}

	/**
	 * On submit.
	 * 
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @param model
	 *            the model
	 * @return the model
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST)
	public Model onSubmit(final HttpServletRequest request,
			final HttpServletResponse response, final Model model)
			throws Exception {
		final String userName = request.getParameter("lastName");
		final String caseId = request.getParameter("case");

		if (request.getParameter("cancel") != null) {
			response.sendRedirect("caseform?caseId=" + caseId);
			return model;
		}

		if (userName != null) {
			final List<User> users = this.userManager.getAll();
			final List<User> res = new LinkedList<User>();
			for (final User u : users) {
				if (u.getLastName().toLowerCase()
						.contains(userName.toLowerCase())) {
					res.add(u);
				}
			}
			model.addAttribute("users", res);
		}

		final String[] userIds = request.getParameterValues("sel[]");

		if (ArrayUtils.isNotEmpty(userIds) && StringUtils.isNotEmpty(caseId)) {
			AlphaCase aCase = this.caseManager.get(caseId);

			for (final String userId : userIds) {
				final User participant = this.userManager.getUser(userId);

				aCase.addParticipant(participant);
			}
			aCase = this.caseManager.save(aCase);
			response.sendRedirect("caseform?caseId=" + caseId);
		}

		return model;
	}
}
