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

import javax.servlet.http.HttpServletRequest;

import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.service.CaseManager;

/**
 * Controller of the case menu.
 */

@Controller
@RequestMapping("/caseMenu")
public class CaseMenuController {

	/** the CaseManager. @see CaseManager CaseManager */
	private CaseManager caseManager;

	/** the UserManager. @see UserManager UserManager */
	private UserManager userManager;

	/**
	 * sets the case manager
	 * 
	 * @param caseManager
	 *            the new CaseManager
	 */
	@Autowired
	public void setCaseManager(
			@Qualifier("caseManager") final CaseManager caseManager) {
		this.caseManager = caseManager;
	}

	/**
	 * sets the user manager
	 * 
	 * @param userManager
	 *            the new UserManager
	 */
	@Autowired
	public void setUserManager(final UserManager userManager) {
		this.userManager = userManager;
	}

	/**
	 * handles the incoming request.
	 * 
	 * @param request
	 *            the request
	 * @return ModelView
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.GET)
	public ModelAndView handleRequest(final HttpServletRequest request)
			throws Exception {

		final User currentUser = this.userManager.getUserByUsername(request
				.getRemoteUser());

		return new ModelAndView().addObject("caseList",
				this.caseManager.findByParticipant(currentUser));

	}
}
