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
package alpha.portal.webapp.listener;

import java.util.Map;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.appfuse.Constants;
import org.springframework.mock.web.MockServletContext;
import org.springframework.web.context.ContextLoader;
import org.springframework.web.context.ContextLoaderListener;
import org.springframework.web.context.WebApplicationContext;

/**
 * This class tests the StartupListener class to verify that variables are
 * placed into the servlet context.
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
public class StartupListenerTest extends TestCase {

	/** The sc. */
	private MockServletContext sc = null;

	/** The listener. */
	private ServletContextListener listener = null;

	/** The spring listener. */
	private ContextLoaderListener springListener = null;

	/*
	 * (non-Javadoc)
	 * 
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		this.sc = new MockServletContext("");
		this.sc.addInitParameter(Constants.CSS_THEME, "simplicity");

		// initialize Spring
		this.sc.addInitParameter(ContextLoader.CONFIG_LOCATION_PARAM,
				"classpath:/applicationContext-dao.xml, "
						+ "classpath:/applicationContext-service.xml, "
						+ "classpath:/applicationContext-resources.xml");

		this.springListener = new ContextLoaderListener();
		this.springListener
				.contextInitialized(new ServletContextEvent(this.sc));
		this.listener = new StartupListener();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
		this.springListener = null;
		this.listener = null;
		this.sc = null;
	}

	/**
	 * Test context initialized.
	 */
	public void testContextInitialized() {
		this.listener.contextInitialized(new ServletContextEvent(this.sc));

		Assert.assertTrue(this.sc.getAttribute(Constants.CONFIG) != null);
		final Map config = (Map) this.sc.getAttribute(Constants.CONFIG);
		Assert.assertEquals(config.get(Constants.CSS_THEME), "simplicity");

		Assert.assertTrue(this.sc
				.getAttribute(WebApplicationContext.ROOT_WEB_APPLICATION_CONTEXT_ATTRIBUTE) != null);
		Assert.assertTrue(this.sc.getAttribute(Constants.AVAILABLE_ROLES) != null);
	}
}
