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
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.GenericManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentRules;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeRange;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.ContributorRoleManager;
import alpha.portal.service.UserExtensionManager;

/**
 * Controller of the adornment form.
 * 
 * @see BaseFormController Base FormController
 */
@Controller
@RequestMapping("/adornmentform*")
public class AdornmentFormController extends BaseFormController {

	/** The alpha card manager. */
	@Autowired
	private AlphaCardManager alphaCardManager;

	/** The contributor role manager. */
	@Autowired
	private ContributorRoleManager contributorRoleManager;

	/** The adornment manager. */
	@Autowired
	private GenericManager<Adornment, Long> adornmentManager;

	/** The user extension manager. */
	@Autowired
	private UserExtensionManager userExtensionManager;

	/**
	 * Must be called in each function of this controller to set allowed
	 * valueRange for specific AdornmentTypes.
	 * 
	 * @param caseId
	 *            of the case of the current card
	 * @param cardId
	 *            of the current card
	 */
	private void setupAdornmentTypes(final String caseId, final String cardId) {
		final List<ContributorRole> roles = this.contributorRoleManager
				.getAll();
		final List<String> roleNames = new LinkedList<String>();
		for (final ContributorRole r : roles) {
			roleNames.add(r.getName());
		}
		AdornmentType.ContributorRole.setValueRange(new AdornmentTypeRange(
				roleNames.toArray(new String[] {})));

		final AlphaCard card = this.alphaCardManager
				.get(new AlphaCardIdentifier(caseId, cardId));
		final Adornment contributorRole = card.getAlphaCardDescriptor()
				.getAdornment(AdornmentType.ContributorRole.getName());

		final List<String> userIds = new LinkedList<String>();
		if ((contributorRole == null)
				|| StringUtils.isBlank(contributorRole.getValue())) {
			final List<User> users = this.getUserManager().getAll();
			for (final User u : users) {
				userIds.add(u.getId().toString());
			}
		} else {
			final List<UserExtension> users = this.userExtensionManager
					.getUserExtensionsByContributorRole(this.contributorRoleManager
							.getContributorRoleByName(contributorRole
									.getValue()));
			for (final UserExtension ue : users) {
				userIds.add(ue.getUserId().toString());
			}
		}
		AdornmentType.Contributor.setValueRange(new AdornmentTypeRange(userIds
				.toArray(new String[] {})));
	}

	/**
	 * Inits the binder.
	 * 
	 * @param binder
	 *            the binder
	 */
	@InitBinder
	public void initBinder(final WebDataBinder binder) {
		binder.setRequiredFields("name", "value");
		binder.setDisallowedFields("adornmentId");
	}

	/**
	 * shows the adornment form.
	 * 
	 * @param request
	 *            the request
	 * @param model
	 *            the model
	 * @return a new adornment
	 * @see adornmentform.jsp
	 */
	@RequestMapping(method = RequestMethod.GET)
	protected String showForm(final HttpServletRequest request,
			final Model model) {
		final String adornmentId = request.getParameter("id");
		final String cardId = request.getParameter("card");
		final String caseId = request.getParameter("case");
		this.setCancelView("redirect:/caseform?activeCardId=" + cardId
				+ "&caseId=" + caseId);
		this.setSuccessView("redirect:/caseform?activeCardId=" + cardId
				+ "&caseId=" + caseId);
		this.setupAdornmentTypes(caseId, cardId);
		final Locale locale = request.getLocale();

		final List<ContributorRole> roles = this.contributorRoleManager
				.getAll();
		model.addAttribute("roles", roles);

		Adornment adornment = new Adornment();
		if (StringUtils.isNotEmpty(adornmentId)) {
			try {
				Long.valueOf(adornmentId);
			} catch (final NumberFormatException e) {
				this.saveError(request,
						this.getText("adornment.invalidId", locale));
				model.addAttribute("adornment", adornment);
				return "redirect:/adornmentform?id=" + adornmentId + "&card="
						+ cardId + "&case=" + caseId;
			}
			adornment = this.adornmentManager.get(Long.valueOf(adornmentId));
			final AdornmentType type = AdornmentType.fromName(adornment
					.getName());
			model.addAttribute("adornmentType", type);

		}

		final AlphaCard card = this.alphaCardManager
				.get(new AlphaCardIdentifier(caseId, cardId));
		final Adornment contributor = card.getAlphaCardDescriptor()
				.getAdornment(AdornmentType.Contributor.getName());

		if ((contributor.getValue() == null)
				|| contributor.getValue().isEmpty()) {

			this.saveError(request, this.getText("adornment.noAccess", locale));
			return "redirect:/caseform?activeCardId=" + cardId + "&caseId="
					+ caseId;

		} else {

			final Long contributorID = Long.parseLong(contributor.getValue());
			final User currentUser = this.getUserManager().getUserByUsername(
					request.getRemoteUser());

			if (contributorID != currentUser.getId()) {

				this.saveError(request,
						this.getText("adornment.noAccess", locale));
				return "redirect:/caseform?activeCardId=" + cardId + "&caseId="
						+ caseId;
			}
		}

		model.addAttribute("adornment", adornment);
		return null;
	}

	/**
	 * handles the case, if the user clicks on one of the buttons.
	 * 
	 * @param newAdornment
	 *            the new adornment
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return success
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST)
	public String onSubmit(final Adornment newAdornment,
			final BindingResult errors, final HttpServletRequest request,
			final HttpServletResponse response) throws Exception {
		if (request.getParameter("cancel") != null)
			return this.getCancelView();

		final String adornmentId = request.getParameter("adornmentId");
		final String cardId = request.getParameter("card");
		final String caseId = request.getParameter("case");
		final String success = this.getSuccessView();
		final Locale locale = request.getLocale();
		this.setupAdornmentTypes(caseId, cardId);

		final AlphaCardIdentifier alphaCardIdentifier = new AlphaCardIdentifier(
				caseId, cardId);
		AlphaCard card = this.alphaCardManager.get(alphaCardIdentifier);

		if (request.getParameter("delete") != null) {
			if (AdornmentType.fromName(newAdornment.getName()) == null) {
				card.getAlphaCardDescriptor().deleteAdornment(
						newAdornment.getName());
				this.saveMessage(request,
						this.getText("adornment.deleted", locale));
			} else {
				newAdornment.setValue("");
				if (AdornmentRules.applyRules(card, newAdornment)) {
					card.getAlphaCardDescriptor().setAdornment(newAdornment);
					this.saveMessage(request,
							this.getText("adornment.deleted", locale));
				} else {
					this.saveError(request,
							this.getText("adornment.errorDelete", locale));
					return this.getCancelView();
				}
			}

		} else {
			if (newAdornment.getName() == AdornmentType.DataProvision.getName()) {
				this.saveError(request,
						this.getText("adornment.errorChange", locale));
				return this.getCancelView();
			}
			if (AdornmentRules.applyRules(card, newAdornment)) {
				final boolean isNew = card.getAlphaCardDescriptor()
						.getAdornment(newAdornment.getName()) == null;
				card.getAlphaCardDescriptor().setAdornment(newAdornment);
				final String key = (isNew) ? "adornment.added"
						: "adornment.updated";
				this.saveMessage(request, this.getText(key, locale));
			} else {
				this.saveError(request,
						this.getText("adornment.errorChange", locale));
				return this.getCancelView();
			}
		}

		// update Data Provision Adornment after any adornment change
		card.getAlphaCardDescriptor().setAdornment(
				AdornmentType.DataProvision.getName(),
				AdornmentRules.getDataProvisionStatus(card));

		if (card.getAlphaCardDescriptor().isAdornmentsChanged()) {
			card = this.alphaCardManager.save(card);
		}

		return success;
	}
}
