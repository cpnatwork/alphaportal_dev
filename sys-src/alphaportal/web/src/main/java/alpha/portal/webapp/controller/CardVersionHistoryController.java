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

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.CaseManager;

/**
 * Controller of the card form.
 * 
 * @see BaseFormController Base FormController
 */
@Controller
@RequestMapping("/cardVersionHistory*")
public class CardVersionHistoryController extends BaseFormController {

	/**
	 * The CaseManager.
	 * 
	 * @see CaseManager CaseManager
	 */
	@Autowired
	private AlphaCardManager cardManager;

	/**
	 * The CaseManager.
	 * 
	 * @see CaseManager CaseManager
	 */
	@Autowired
	private CaseManager caseManager;

	/**
	 * Show form.
	 * 
	 * @param m
	 *            the m
	 * @param request
	 *            the request
	 * @return the string
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	@RequestMapping(method = RequestMethod.GET)
	protected String showForm(final Model m, final HttpServletRequest request)
			throws IOException {

		final String caseId = request.getParameter("case");
		final String version = request.getParameter("version");

		String paging = "";
		for (final Object e : request.getParameterMap().keySet()) {
			if ((e.toString().length() > 2)
					&& e.toString().substring(0, 2).equalsIgnoreCase("d-")) {
				paging = e.toString() + "="
						+ request.getParameter(e.toString());
				break;
			}
		}
		m.addAttribute("paging", paging);

		Long versionL = null;
		if (StringUtils.isNotBlank(version)) {
			try {
				versionL = Long.parseLong(version);
			} catch (final NumberFormatException e) {
				this.saveError(request, "card.invalidVersion");
				return "redirect:/caseform?caseId=" + caseId;
			}
		}

		if (StringUtils.isBlank(caseId) || !this.caseManager.exists(caseId)) {
			this.saveError(request, "case.invalidId");
			return "redirect:/caseMenu";
		}

		final AlphaCase alphaCase = this.caseManager.get(caseId);
		final List<AlphaCard> versions = this.cardManager
				.getAllVersions(caseId);
		m.addAttribute("case", alphaCase);
		m.addAttribute("cards", versions);

		AlphaCard activeCard = null;
		final User user = this.getUserManager().getUserByUsername(
				request.getRemoteUser());
		for (final AlphaCard c : versions) {
			if (c.getAlphaCardIdentifier().getSequenceNumber() == versionL) {
				final Adornment contributor = c.getAlphaCardDescriptor()
						.getAdornment(AdornmentType.Contributor.getName());
				if ((user == null)
						|| ((contributor != null) && !contributor.getValue()
								.equals(user.getId().toString()))) {
					this.saveError(request, "card.invalidId");
					return "cardVersionHistory";
				}
				activeCard = c;
				break;
			}
		}
		m.addAttribute("activeCard", activeCard);

		return "cardVersionHistory";
	}
}
