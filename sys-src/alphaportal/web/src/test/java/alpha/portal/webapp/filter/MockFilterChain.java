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

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Borrowed from the Display Tag project:
 * http://displaytag.sourceforge.net/xref-
 * test/org/displaytag/filter/MockFilterSupport.html
 * 
 * Todo: look into using Spring's MockFilterChain:
 * http://www.springframework.org
 * /docs/api/org/springframework/mock/web/MockFilterChain.html
 */
public class MockFilterChain implements FilterChain {

	/** The log. */
	private final Log log = LogFactory.getLog(MockFilterChain.class);

	/** The forward url. */
	private String forwardURL;

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.servlet.FilterChain#doFilter(javax.servlet.ServletRequest,
	 * javax.servlet.ServletResponse)
	 */
	public void doFilter(final ServletRequest request,
			final ServletResponse response) throws IOException,
			ServletException {
		String uri = ((HttpServletRequest) request).getRequestURI();
		final String requestContext = ((HttpServletRequest) request)
				.getContextPath();

		if (StringUtils.isNotEmpty(requestContext)
				&& uri.startsWith(requestContext)) {
			uri = uri.substring(requestContext.length());
		}

		this.forwardURL = uri;
		this.log.debug("Forwarding to: " + uri);

		final RequestDispatcher dispatcher = request.getRequestDispatcher(uri);
		dispatcher.forward(request, response);
	}

	/**
	 * Gets the forward url.
	 * 
	 * @return the forward url
	 */
	public String getForwardURL() {
		return this.forwardURL;
	}
}