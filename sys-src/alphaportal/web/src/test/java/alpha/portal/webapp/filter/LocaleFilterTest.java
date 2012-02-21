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
package alpha.portal.webapp.filter;

import java.util.Locale;

import javax.servlet.jsp.jstl.core.Config;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.appfuse.Constants;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.mock.web.MockFilterConfig;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

/**
 * The Class LocaleFilterTest.
 */
public class LocaleFilterTest extends TestCase {

	/** The filter. */
	private LocaleFilter filter = null;

	/*
	 * (non-Javadoc)
	 * 
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		this.filter = new LocaleFilter();
		this.filter.init(new MockFilterConfig());
	}

	/**
	 * Test set locale in session when session is null.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void testSetLocaleInSessionWhenSessionIsNull() throws Exception {
		final MockHttpServletRequest request = new MockHttpServletRequest();
		request.addParameter("locale", "es");

		final MockHttpServletResponse response = new MockHttpServletResponse();
		this.filter.doFilter(request, response, new MockFilterChain());

		// no session, should result in null
		Assert.assertNull(request.getSession().getAttribute(
				Constants.PREFERRED_LOCALE_KEY));
		// thread locale should always have it, regardless of session
		Assert.assertNotNull(LocaleContextHolder.getLocale());
	}

	/**
	 * Test set locale in session when session not null.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void testSetLocaleInSessionWhenSessionNotNull() throws Exception {
		final MockHttpServletRequest request = new MockHttpServletRequest();
		request.addParameter("locale", "es");

		final MockHttpServletResponse response = new MockHttpServletResponse();
		request.setSession(new MockHttpSession(null));

		this.filter.doFilter(request, response, new MockFilterChain());

		// session not null, should result in not null
		final Locale locale = (Locale) request.getSession().getAttribute(
				Constants.PREFERRED_LOCALE_KEY);
		Assert.assertNotNull(locale);
		Assert.assertNotNull(LocaleContextHolder.getLocale());
		Assert.assertEquals(new Locale("es"), locale);
	}

	/**
	 * Test set invalid locale.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void testSetInvalidLocale() throws Exception {
		final MockHttpServletRequest request = new MockHttpServletRequest();
		request.addParameter("locale", "foo");

		final MockHttpServletResponse response = new MockHttpServletResponse();
		request.setSession(new MockHttpSession(null));

		this.filter.doFilter(request, response, new MockFilterChain());

		// a locale will get set regardless - there's no such thing as an
		// invalid one
		Assert.assertNotNull(request.getSession().getAttribute(
				Constants.PREFERRED_LOCALE_KEY));
	}

	/**
	 * Test jstl locale is set.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void testJstlLocaleIsSet() throws Exception {
		final MockHttpServletRequest request = new MockHttpServletRequest();
		request.addParameter("locale", "es");

		final MockHttpServletResponse response = new MockHttpServletResponse();
		request.setSession(new MockHttpSession(null));

		this.filter.doFilter(request, response, new MockFilterChain());

		Assert.assertNotNull(Config.get(request.getSession(), Config.FMT_LOCALE));
	}

	/**
	 * Test locale and country.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public void testLocaleAndCountry() throws Exception {
		final MockHttpServletRequest request = new MockHttpServletRequest();
		request.setSession(new MockHttpSession());
		request.addParameter("locale", "zh_TW");

		final MockHttpServletResponse response = new MockHttpServletResponse();
		this.filter.doFilter(request, response, new MockFilterChain());

		// session not null, should result in not null
		final Locale locale = (Locale) request.getSession().getAttribute(
				Constants.PREFERRED_LOCALE_KEY);
		Assert.assertNotNull(locale);
		Assert.assertEquals(new Locale("zh", "TW"), locale);
	}
}
