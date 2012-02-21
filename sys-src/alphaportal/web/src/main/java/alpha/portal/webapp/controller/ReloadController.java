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

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.webapp.listener.StartupListener;

/**
 * This class is used to reload the drop-downs initialized in the
 * StartupListener.
 * 
 * <p>
 * <a href="ReloadController.java.html"><i>View Source</i></a>
 * </p>
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
@Controller
@RequestMapping("/admin/reload*")
public class ReloadController {

	/** The log. */
	private transient final Log log = LogFactory.getLog(ReloadController.class);

	/**
	 * Handle request.
	 * 
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return the model and view
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.GET)
	@SuppressWarnings("unchecked")
	public ModelAndView handleRequest(final HttpServletRequest request,
			final HttpServletResponse response) throws Exception {
		if (this.log.isDebugEnabled()) {
			this.log.debug("Entering 'execute' method");
		}

		StartupListener.setupContext(request.getSession().getServletContext());

		final String referer = request.getHeader("Referer");

		if (referer != null) {
			this.log.info("reload complete, reloading user back to: " + referer);
			List<String> messages = (List) request.getSession().getAttribute(
					BaseFormController.MESSAGES_KEY);

			if (messages == null) {
				messages = new ArrayList();
			}

			messages.add("Reloading options completed successfully.");
			request.getSession().setAttribute(BaseFormController.MESSAGES_KEY,
					messages);

			response.sendRedirect(response.encodeRedirectURL(referer));
			return null;
		} else {
			response.setContentType("text/html");

			final PrintWriter out = response.getWriter();

			out.println("<html>");
			out.println("<head>");
			out.println("<title>Context Reloaded</title>");
			out.println("</head>");
			out.println("<body bgcolor=\"white\">");
			out.println("<script type=\"text/javascript\">");
			out.println("alert('Context Reload Succeeded! Click OK to continue.');");
			out.println("history.back();");
			out.println("</script>");
			out.println("</body>");
			out.println("</html>");
		}

		return null;
	}

}
