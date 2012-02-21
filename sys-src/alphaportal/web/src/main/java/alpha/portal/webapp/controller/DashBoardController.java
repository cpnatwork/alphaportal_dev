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

import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.GenericManager;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.ContributorRequest;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.CaseManager;

/**
 * The Class DashBoardController.
 */
@Controller
@RequestMapping("/dashBoard*")
public class DashBoardController {

	/** The alpha card manager. */
	@Autowired
	private AlphaCardManager alphaCardManager;

	/** The user manager. */
	@Autowired
	private UserManager userManager;

	/** The case manager. */
	@Autowired
	private CaseManager caseManager;

	/** The card manager. */
	@Autowired
	private AlphaCardManager cardManager;

	/** The contr req manager. */
	@Autowired
	private GenericManager<ContributorRequest, Long> contrReqManager;

	/**
	 * Show form.
	 * 
	 * @param request
	 *            the request
	 * @return the model and view
	 */
	@RequestMapping(method = RequestMethod.GET)
	protected ModelAndView showForm(final HttpServletRequest request) {
		final ModelAndView model = new ModelAndView("dashBoard");
		AlphaCard activeCard = null;

		final String version = request.getParameter("version");
		Long versionL = null;
		if (StringUtils.isNotBlank(version)) {
			try {
				versionL = Long.parseLong(version);
			} catch (final NumberFormatException e) {

			}
		}

		// get the current user
		final User currentUser = this.userManager.getUserByUsername(request
				.getRemoteUser());

		final List<AlphaCase> caseList = this.caseManager
				.findByParticipant(currentUser);

		final List<AlphaCard> cardsList = this.cardManager
				.listDashBoardCards(caseList);

		for (final AlphaCard c : cardsList) {
			if (c.getAlphaCardIdentifier().getSequenceNumber().equals(versionL)) {
				activeCard = c;
				break;
			}
		}

		final List<ContributorRequest> contrReqList = this.contrReqManager
				.getAll();
		final List<ContributorRequest> newList = new LinkedList<ContributorRequest>();

		if (!contrReqList.isEmpty()) {
			for (final ContributorRequest req : contrReqList) {
				if (currentUser.getId().equals(req.getAcceptingUser().getId())) {
					newList.add(req);
				}
			}
		}

		model.addObject("requests", newList);

		model.addObject("cards", cardsList);
		model.addObject("activeCard", activeCard);

		return model;
	}

	/**
	 * Accept request.
	 * 
	 * @param contributorRequest
	 *            the contributor request
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "acceptRequest" })
	public String acceptRequest(final ContributorRequest contributorRequest,
			final BindingResult errors, final HttpServletRequest request)
			throws Exception {

		// get the current user
		final String reqId = request.getParameter("sel");

		final ContributorRequest contrReq = this.contrReqManager.get(Long
				.parseLong(reqId));
		if (contrReq == null) {
			// TODO Error
		}

		final User currentUser = this.userManager.getUserByUsername(request
				.getRemoteUser());

		final AlphaCard aCard = contrReq.getAlphaCard();

		aCard.getAlphaCase().addParticipant(currentUser);
		this.caseManager.save(aCard.getAlphaCase());
		aCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Contributor.getName(),
				currentUser.getId().toString());
		this.alphaCardManager.save(aCard);

		this.contrReqManager.remove(Long.parseLong(reqId));

		return "redirect:/dashBoard";
	}

	/**
	 * Deny request.
	 * 
	 * @param contributorRequest
	 *            the contributor request
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "denyRequest" })
	public String denyRequest(final ContributorRequest contributorRequest,
			final BindingResult errors, final HttpServletRequest request)
			throws Exception {

		// get the current user
		final String reqId = request.getParameter("sel");

		final ContributorRequest contrReq = this.contrReqManager.get(Long
				.parseLong(reqId));
		if (contrReq == null) {
			// TODO Error
		}

		this.contrReqManager.remove(Long.parseLong(reqId));

		return "redirect:/dashBoard";
	}
}
