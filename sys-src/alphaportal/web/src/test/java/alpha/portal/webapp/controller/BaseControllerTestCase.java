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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Before;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.AbstractTransactionalJUnit4SpringContextTests;

/**
 * The Class BaseControllerTestCase.
 */
@ContextConfiguration(locations = {
		"classpath:/applicationContext-resources.xml",
		"classpath:/applicationContext-dao.xml",
		"classpath:/applicationContext-service.xml",
		"classpath*:/applicationContext.xml", // for modular archetypes
		"/WEB-INF/applicationContext*.xml", "/WEB-INF/dispatcher-servlet.xml" })
public abstract class BaseControllerTestCase extends
		AbstractTransactionalJUnit4SpringContextTests {

	/** The log. */
	protected transient final Log log = LogFactory.getLog(this.getClass());

	/** The smtp port. */
	private int smtpPort = 25250;

	/**
	 * On set up.
	 */
	@Before
	public void onSetUp() {
		this.smtpPort = this.smtpPort + (int) (Math.random() * 100);
		// change the port on the mailSender so it doesn't conflict with an
		// existing SMTP server on localhost
		final JavaMailSenderImpl mailSender = (JavaMailSenderImpl) this.applicationContext
				.getBean("mailSender");
		mailSender.setPort(this.getSmtpPort());
		mailSender.setHost("localhost");
	}

	/**
	 * Gets the smtp port.
	 * 
	 * @return the smtp port
	 */
	protected int getSmtpPort() {
		return this.smtpPort;
	}

	/**
	 * Convenience methods to make tests simpler.
	 * 
	 * @param url
	 *            the URL to post to
	 * @return a MockHttpServletRequest with a POST to the specified URL
	 */
	public MockHttpServletRequest newPost(final String url) {
		return new MockHttpServletRequest("POST", url);
	}

	/**
	 * New get.
	 * 
	 * @param url
	 *            the url
	 * @return the mock http servlet request
	 */
	public MockHttpServletRequest newGet(final String url) {
		return new MockHttpServletRequest("GET", url);
	}
}
