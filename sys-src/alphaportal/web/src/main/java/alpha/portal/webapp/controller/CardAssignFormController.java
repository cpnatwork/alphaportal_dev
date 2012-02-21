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
import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.GenericManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessException;
import org.springframework.orm.ObjectRetrievalFailureException;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.ContributorRequest;
import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.ContributorRoleManager;
import alpha.portal.service.UserExtensionManager;

/**
 * The Class CardAssignFormController.
 */
@Controller
@RequestMapping("/cardassignform*")
public class CardAssignFormController extends BaseFormController {

	/** The user extension manager. */
	@Autowired
	private UserExtensionManager userExtensionManager;

	/** The card manager. */
	@Autowired
	private AlphaCardManager cardManager;

	/** The contributor role manager. */
	@Autowired
	private ContributorRoleManager contributorRoleManager;

	/** The contr req manager. */
	@Autowired
	private GenericManager<ContributorRequest, Long> contrReqManager;

	/**
	 * Handles the viewing of all users who have the card's ContributorRole.
	 * 
	 * @param request
	 *            the request
	 * @param model
	 *            the model
	 */
	@RequestMapping(method = RequestMethod.GET)
	protected void showForm(final HttpServletRequest request, final Model model) {
		final String caseId = request.getParameter("case");
		final String cardId = request.getParameter("card");
		AlphaCard card = null;
		try {
			card = this.cardManager
					.get(new AlphaCardIdentifier(caseId, cardId));
		} catch (final ObjectRetrievalFailureException e) {
			this.saveError(request,
					this.getText("cardassign.noCard", request.getLocale()));
		}
		if (card == null)
			return;

		final Adornment contributorRole = card.getAlphaCardDescriptor()
				.getAdornment(AdornmentType.ContributorRole.getName());
		if (contributorRole == null) {
			this.saveError(
					request,
					this.getText("cardassign.noContributorRole",
							request.getLocale()));
			return;
		}

		final ContributorRole role = this.contributorRoleManager
				.getContributorRoleByName(contributorRole.getValue());
		List<UserExtension> users = new LinkedList<UserExtension>();
		if ((role == null) || StringUtils.isBlank(role.getName())) {
			final List<User> list = this.getUserManager().getAll();
			for (final User u : list) {
				users.add(new UserExtension(u));
			}
		} else {
			users = this.userExtensionManager
					.getUserExtensionsByContributorRole(role);
		}

		model.addAttribute("users", users);
	}

	/**
	 * Return to case.
	 * 
	 * @param response
	 *            the response
	 * @param caseId
	 *            the case id
	 * @param cardId
	 *            the card id
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	private void returnToCase(final HttpServletResponse response,
			final String caseId, final String cardId) throws IOException {
		response.sendRedirect("caseform?caseId=" + caseId + "&activeCardId="
				+ cardId);
	}

	/**
	 * Adds the selected user to the case's participants and sets him as
	 * Contributor for the card. Also returns to caseform on cancel.
	 * 
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST)
	public void onSubmit(final HttpServletRequest request,
			final HttpServletResponse response) throws Exception {
		final String caseId = request.getParameter("case");
		final String cardId = request.getParameter("card");
		final String userId = request.getParameter("user");

		if (request.getParameter("cancel") != null) {
			this.returnToCase(response, caseId, cardId);
			return;
		}

		if (StringUtils.isBlank(userId)) {
			this.saveError(request,
					this.getText("cardassign.noUser", request.getLocale()));
			this.returnToCase(response, caseId, cardId);
			return;
		}

		Long userIdLong = null;
		try {
			userIdLong = Long.parseLong(userId);
		} catch (final NumberFormatException e) {
			this.saveError(request, "cardassign.invalidUser");
			this.returnToCase(response, caseId, cardId);
			return;
		}

		if (userIdLong == null) {
			this.saveMessage(request,
					this.getText("cardassign.invalidUser", request.getLocale()));
			this.returnToCase(response, caseId, cardId);
			return;
		}

		final AlphaCard card = this.cardManager.get(new AlphaCardIdentifier(
				caseId, cardId));

		UserExtension ue = null;
		try {
			ue = this.userExtensionManager.get(userIdLong);
		} catch (final DataAccessException e) {
		}
		if (ue == null) {
			try {
				final User u = this.getUserManager().get(userIdLong);
				ue = new UserExtension(u);
				ue = this.userExtensionManager.save(ue);
			} catch (final DataAccessException e) {
				this.saveError(request, "cardassign.invalidUser");
				this.returnToCase(response, caseId, cardId);
				return;
			}
		}

		ContributorRequest contrRequest = new ContributorRequest(this
				.getUserManager().getUserByUsername(request.getRemoteUser()),
				this.getUserManager().get(userIdLong), card);
		contrRequest = this.contrReqManager.save(contrRequest);

		response.sendRedirect("caseform?caseId=" + caseId + "&activeCardId="
				+ cardId);
	}
}
