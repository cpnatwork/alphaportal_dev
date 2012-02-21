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

import java.util.List;
import java.util.Map;

import org.appfuse.Constants;
import org.compass.gps.CompassGps;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.servlet.ModelAndView;

/**
 * The Class UserControllerTest.
 */
public class UserControllerTest extends BaseControllerTestCase {

	/** The compass gps. */
	@Autowired
	private CompassGps compassGps;

	/** The c. */
	@Autowired
	private UserController c;

	/**
	 * Test handle request.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testHandleRequest() throws Exception {
		final ModelAndView mav = this.c.handleRequest(null);
		final Map m = mav.getModel();
		Assert.assertNotNull(m.get(Constants.USER_LIST));
		Assert.assertEquals("admin/userList", mav.getViewName());
	}

	/**
	 * Test search.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testSearch() throws Exception {
		this.compassGps.index();
		final ModelAndView mav = this.c.handleRequest("admin");
		final Map m = mav.getModel();
		final List results = (List) m.get(Constants.USER_LIST);
		Assert.assertNotNull(results);
		Assert.assertTrue(results.size() >= 1);
		Assert.assertEquals("admin/userList", mav.getViewName());
	}
}
