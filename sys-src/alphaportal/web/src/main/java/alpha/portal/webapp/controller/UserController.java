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

import org.appfuse.Constants;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;

/**
 * Simple class to retrieve a list of users from the database.
 * <p/>
 * <p>
 * <a href="UserController.java.html"><i>View Source</i></a>
 * </p>
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
@Controller
@RequestMapping("/admin/users*")
public class UserController {

	/** The mgr. */
	private UserManager mgr = null;

	/**
	 * Sets the user manager.
	 * 
	 * @param userManager
	 *            the new user manager
	 */
	@Autowired
	public void setUserManager(final UserManager userManager) {
		this.mgr = userManager;
	}

	/**
	 * Handle request.
	 * 
	 * @param query
	 *            the query
	 * @return the model and view
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.GET)
	public ModelAndView handleRequest(
			@RequestParam(required = false, value = "q") final String query)
			throws Exception {
		return new ModelAndView("admin/userList", Constants.USER_LIST,
				this.mgr.search(query));
	}
}
