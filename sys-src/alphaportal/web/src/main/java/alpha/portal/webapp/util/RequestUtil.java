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
package alpha.portal.webapp.util;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Convenience class for setting and retrieving cookies.
 */
public final class RequestUtil {

	/** The Constant log. */
	private static final Log log = LogFactory.getLog(RequestUtil.class);

	/**
	 * Checkstyle rule: utility classes should not have public constructor.
	 */
	private RequestUtil() {
	}

	/**
	 * Convenience method to set a cookie.
	 * 
	 * @param response
	 *            the current response
	 * @param name
	 *            the name of the cookie
	 * @param value
	 *            the value of the cookie
	 * @param path
	 *            the path to set it on
	 */
	public static void setCookie(final HttpServletResponse response,
			final String name, final String value, final String path) {
		if (RequestUtil.log.isDebugEnabled()) {
			RequestUtil.log.debug("Setting cookie '" + name + "' on path '"
					+ path + "'");
		}

		final Cookie cookie = new Cookie(name, value);
		cookie.setSecure(false);
		cookie.setPath(path);
		cookie.setMaxAge(3600 * 24 * 30); // 30 days

		response.addCookie(cookie);
	}

	/**
	 * Convenience method to get a cookie by name.
	 * 
	 * @param request
	 *            the current request
	 * @param name
	 *            the name of the cookie to find
	 * @return the cookie (if found), null if not found
	 */
	public static Cookie getCookie(final HttpServletRequest request,
			final String name) {
		final Cookie[] cookies = request.getCookies();
		Cookie returnCookie = null;

		if (cookies == null)
			return returnCookie;

		for (final Cookie thisCookie : cookies) {
			if (thisCookie.getName().equals(name)
					&& !"".equals(thisCookie.getValue())) {
				returnCookie = thisCookie;
				break;
			}
		}

		return returnCookie;
	}

	/**
	 * Convenience method for deleting a cookie by name.
	 * 
	 * @param response
	 *            the current web response
	 * @param cookie
	 *            the cookie to delete
	 * @param path
	 *            the path on which the cookie was set (i.e. /appfuse)
	 */
	public static void deleteCookie(final HttpServletResponse response,
			final Cookie cookie, final String path) {
		if (cookie != null) {
			// Delete the cookie by setting its maximum age to zero
			cookie.setMaxAge(0);
			cookie.setPath(path);
			response.addCookie(cookie);
		}
	}

	/**
	 * Convenience method to get the application's URL based on request
	 * variables.
	 * 
	 * @param request
	 *            the current request
	 * @return URL to application
	 */
	public static String getAppURL(final HttpServletRequest request) {
		if (request == null)
			return "";

		final StringBuffer url = new StringBuffer();
		int port = request.getServerPort();
		if (port < 0) {
			port = 80; // Work around java.net.URL bug
		}
		final String scheme = request.getScheme();
		url.append(scheme);
		url.append("://");
		url.append(request.getServerName());
		if ((scheme.equals("http") && (port != 80))
				|| (scheme.equals("https") && (port != 443))) {
			url.append(':');
			url.append(port);
		}
		url.append(request.getContextPath());
		return url.toString();
	}
}
