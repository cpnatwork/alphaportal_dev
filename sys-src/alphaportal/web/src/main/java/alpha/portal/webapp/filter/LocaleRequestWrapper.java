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

import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * HttpRequestWrapper overriding methods getLocale(), getLocales() to include
 * the user's preferred locale.
 */
public class LocaleRequestWrapper extends HttpServletRequestWrapper {

	/** The log. */
	private final transient Log log = LogFactory
			.getLog(LocaleRequestWrapper.class);

	/** The preferred locale. */
	private final Locale preferredLocale;

	/**
	 * Sets preferred local to user's locale.
	 * 
	 * @param decorated
	 *            the current decorated request
	 * @param userLocale
	 *            the user's locale
	 */
	public LocaleRequestWrapper(final HttpServletRequest decorated,
			final Locale userLocale) {
		super(decorated);
		this.preferredLocale = userLocale;
		if (null == this.preferredLocale) {
			this.log.error("preferred locale = null, it is an unexpected value!");
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Locale getLocale() {
		if (null != this.preferredLocale)
			return this.preferredLocale;
		else
			return super.getLocale();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Enumeration<Locale> getLocales() {
		if (null != this.preferredLocale) {
			final List<Locale> l = Collections.list(super.getLocales());
			if (l.contains(this.preferredLocale)) {
				l.remove(this.preferredLocale);
			}
			l.add(0, this.preferredLocale);
			return Collections.enumeration(l);
		} else
			return super.getLocales();
	}

}
