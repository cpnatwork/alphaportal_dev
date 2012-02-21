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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.GenericManager;
import org.appfuse.service.UserManager;
import org.hibernate.criterion.Criterion;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeDeleted;
import alpha.portal.model.AdornmentTypeVisibility;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.model.UserSession;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.CaseManager;
import alpha.portal.service.ContributorRoleManager;
import alpha.portal.service.UserExtensionManager;
import alpha.portal.service.impl.AlphaCardManagerImpl;
import alpha.portal.webapp.util.CardFilterContributor;
import alpha.portal.webapp.util.CardFilterContributorRole;
import alpha.portal.webapp.util.CardFilterDataProvision;
import alpha.portal.webapp.util.CardFilterDeleted;
import alpha.portal.webapp.util.CardFilterHolder;

/**
 * Controller of the case form.
 * 
 * @see BaseFormController Base FormController
 */
@Controller
@RequestMapping("/caseform*")
public class CaseFormController extends BaseFormController {

	/** The Constant CARD_PRIORITY_PARAM_NAME. */
	private static final String CARD_PRIORITY_PARAM_NAME = "cardPriority";

	/**
	 * The CaseManager.
	 * 
	 * @see CaseManager CaseManager
	 */
	@Autowired
	private CaseManager caseManager;

	/**
	 * The UserManager.
	 * 
	 * @see UserManager UserManager
	 */
	@Autowired
	private UserManager userManager;

	/** The user extension manager. */
	@Autowired
	private UserExtensionManager userExtensionManager;

	/** The contributor role manager. */
	@Autowired
	private ContributorRoleManager contributorRoleManager;

	/**
	 * the AlphaCardManager.
	 * 
	 * @see AlphaCardManager AlphaCardManager
	 */
	@Autowired
	private AlphaCardManager alphaCardManager;

	/** The UserSessionManager. */
	@Autowired
	private GenericManager<UserSession, Long> userSessionManager;

	/**
	 * Default constructor, who sets the Cancel- and the Success-View.
	 */
	public CaseFormController() {
		this.setCancelView("redirect:/caseMenu");
		this.setSuccessView("redirect:/caseMenu");
	}

	/**
	 * shows the case form.
	 * 
	 * @param filters
	 *            the filters
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return ModelView
	 * @throws Exception
	 *             the exception
	 * @see caseform.jsp
	 */
	@ModelAttribute("activeCard")
	@RequestMapping(method = RequestMethod.GET)
	protected ModelAndView showForm(final CardFilterHolder filters,
			final HttpServletRequest request, final HttpServletResponse response)
			throws Exception {
		AlphaCard activeCard = null;
		User currentUser = null;
		final String caseId = request.getParameter("caseId");
		final String activeCardId = request.getParameter("activeCardId");

		final ModelAndView m = new ModelAndView("caseform");

		if (request.getParameter("isMyWorklist") != null) {
			m.addObject("isMyWorklist", true);
			filters.setContributor(CardFilterContributor.OWN);
			filters.setDataProvision(CardFilterDataProvision.NOTFULFILLED);

			filters.setContributorRole(CardFilterContributorRole.ALL);
			filters.setShowDeleted(CardFilterDeleted.NOTDELETED);
		}

		/**
		 * Merge filters with Session
		 */
		filters.mergeFiltersWithSession(request, response);

		if (!StringUtils.isBlank(caseId)
				&& (caseId.equals("last") || this.caseManager.exists(caseId))) {

			currentUser = this.userManager.getUserByUsername(request
					.getRemoteUser());
			UserSession userSession;
			if (this.userSessionManager.exists(currentUser.getId())) {
				userSession = this.userSessionManager.get(currentUser.getId());
			} else {
				userSession = new UserSession();
				userSession.setUserId(currentUser.getId());
			}
			m.addObject("currentUserId", currentUser.getId());

			AlphaCase apCase = null;
			// show last viewed case
			if (caseId.equals("last")) {
				final String lastCaseId = userSession.getLastViewedCaseId();
				if (StringUtils.isBlank(lastCaseId)
						|| !this.caseManager.exists(lastCaseId)) {
					// redirect to list
					response.sendRedirect("caseMenu");
				} else {
					apCase = this.caseManager.get(lastCaseId);
				}

			} else {
				apCase = this.caseManager.get(caseId);
				if ((apCase != null)
						&& !StringUtils.isBlank(apCase.getCaseId())) {
					userSession.setLastViewedCaseId(apCase.getCaseId());
					this.userSessionManager.save(userSession);
				}
			}

			m.addObject("case", apCase);
			if (apCase != null) {
				this.setSuccessView("redirect:/caseform?caseId="
						+ apCase.getCaseId());
				m.addObject("cards",
						this.filterAlphaCards(apCase, filters, currentUser));

				m.addObject("participants", apCase.getListOfParticipants());

				final AlphaCardIdentifier activeCardIdentifier = new AlphaCardIdentifier(
						caseId, activeCardId);
				if (!StringUtils.isBlank(activeCardId)) {

					if (this.alphaCardManager.exists(activeCardIdentifier)) {
						activeCard = this.alphaCardManager
								.get(activeCardIdentifier);
						m.addObject("activeCard", activeCard);

						final Adornment deletedAdornment = activeCard
								.getAlphaCardDescriptor().getAdornment(
										AdornmentType.Deleted.getName());
						if (deletedAdornment != null) {
							if (deletedAdornment.getValue().equals(
									AdornmentTypeDeleted.TRUE.value())) {
								m.addObject("activeCardIsDeleted", true);
							}
						}

						boolean hidePayload = false;

						final Adornment contrbitorAdornment = activeCard
								.getAlphaCardDescriptor().getAdornment(
										AdornmentType.Contributor.getName());

						final Adornment visibilityAdornment = activeCard
								.getAlphaCardDescriptor().getAdornment(
										AdornmentType.Visibility.getName());

						if ((contrbitorAdornment != null)
								&& (visibilityAdornment != null)) {
							final String cId = contrbitorAdornment.getValue();
							final String vis = visibilityAdornment.getValue();

							if ((cId != null) && !cId.isEmpty()) {

								final Long contributorID = Long
										.parseLong(activeCard
												.getAlphaCardDescriptor()
												.getAdornment(
														AdornmentType.Contributor
																.getName())
												.getValue());

								if (vis.equals(AdornmentTypeVisibility.PRIVATE
										.value())
										&& (contributorID != null)
										&& !(currentUser.getId() == contributorID)) {

									hidePayload = true;
								}
							} else {

								if (activeCard
										.getAlphaCardDescriptor()
										.getAdornment(
												AdornmentType.Visibility
														.getName())
										.getValue()
										.equals(AdornmentTypeVisibility.PRIVATE
												.value())) {

									hidePayload = true;
								}
							}
						}

						m.addObject("hidePayload", hidePayload);

						boolean currentUserMatchesContributorRole = false;
						if (StringUtils.isBlank(activeCard
								.getAlphaCardDescriptor().getContributorRole())) {
							currentUserMatchesContributorRole = true;
						}
						if (!currentUserMatchesContributorRole) {
							final ContributorRole role = this.contributorRoleManager
									.getContributorRoleByName(activeCard
											.getAlphaCardDescriptor()
											.getContributorRole());
							if (role == null) {
								currentUserMatchesContributorRole = true;
							} else if (this.userExtensionManager
									.exists(currentUser.getId())) {
								final UserExtension ue = this.userExtensionManager
										.get(currentUser.getId());
								if (ue != null) {
									currentUserMatchesContributorRole = ue
											.hasRole(role);
								}
							}
						}
						m.addObject("currentUserMatchesContributorRole",
								currentUserMatchesContributorRole);
						m.addObject("currentUserIsContributor", currentUser
								.getId() == activeCard.getAlphaCardDescriptor()
								.getContributor());

						// new gui stuff
						final Set<String> userRoleStrings = new HashSet<String>();
						if (this.userExtensionManager.exists(currentUser
								.getId())) {
							final Set<ContributorRole> UserRoles = this.userExtensionManager
									.get(currentUser.getId()).getRoles();
							for (final ContributorRole contributorRole : UserRoles) {
								userRoleStrings.add(contributorRole.getName());
							}
						}
						m.addObject("currentUserContributorRoles",
								userRoleStrings.toArray(new String[] {}));

						this.setSuccessView("redirect:/caseform?caseId="
								+ apCase.getCaseId() + "&activeCardId="
								+ activeCardIdentifier.getCardId());

					} else if (activeCardId.equals("new")) {
						m.addObject("activeCard",
								this.alphaCardManager.createAlphaCard(caseId));

					}
				}

			}

			// Filters
			m.addObject("filters", filters);

			// Essential Adornments
			final List<String> essential = new LinkedList<String>();
			essential.add(AdornmentType.Title.getName());
			essential.add(AdornmentType.Contributor.getName());
			essential.add(AdornmentType.ContributorRole.getName());
			m.addObject("essentialAdornments",
					essential.toArray(new String[] {}));

		} else {
			m.addObject("case", new AlphaCase());
		}
		return m;
	}

	/**
	 * Filter alpha cards.
	 * 
	 * @param apCase
	 *            the ap case
	 * @param filters
	 *            the filters
	 * @param user
	 *            the user
	 * @return the list
	 */
	public List<AlphaCard> filterAlphaCards(final AlphaCase apCase,
			final CardFilterHolder filters, final User user) {

		final CardFilterContributor contributor = filters.getContributor();
		final CardFilterContributorRole contributorRole = filters
				.getContributorRole();
		final CardFilterDataProvision dataProvision = filters
				.getDataProvision();
		final CardFilterDeleted showDeleted = filters.getShowDeleted();

		UserExtension userExtension = null;
		try {
			userExtension = this.userExtensionManager.get(user.getId());
		} catch (final Exception e) {
			userExtension = new UserExtension();
		}

		final List<Criterion> list = new ArrayList<Criterion>();
		final Set<String> setStr = new HashSet<String>();

		final Set<ContributorRole> set = userExtension.getRoles();

		for (final ContributorRole tmp : set) {
			setStr.add(tmp.getName());
		}

		if (contributor != null) {
			switch (contributor) {
			case OTHERS:

				list.add(AlphaCardManagerImpl
						.getContributorCriterionOthers(user.getId().toString()));
				break;
			case OWN:

				list.add(AlphaCardManagerImpl.getContributorCriterionOwn(user
						.getId().toString()));

			default:
				break;
			}
		}

		if ((contributorRole != null) && !setStr.isEmpty()) {
			switch (contributorRole) {
			case OTHERS:

				list.add(AlphaCardManagerImpl
						.getContributorRoleCriterionOthers(setStr
								.toArray(new String[0])));
				setStr.add("");
				break;
			case OWN:

				list.add(AlphaCardManagerImpl
						.getContributorRoleCriterionOwn(setStr
								.toArray(new String[0])));
			default:
				break;
			}
		} else if ((contributorRole != null) && setStr.isEmpty()) {
			setStr.add("0");
		}

		if (dataProvision != null) {
			switch (dataProvision) {
			case FULLFILLED:

				list.add(AlphaCardManagerImpl.DATA_PROVISION_FULFILLED);
				break;
			case INPROGRESS:

				list.add(AlphaCardManagerImpl.DATA_PROVISION_INPROGRESS);
				break;
			case NOTFULFILLED:

				list.add(AlphaCardManagerImpl.DATA_PROVISION_OPEN_INPROGRESS);
				break;
			case OPEN:

				list.add(AlphaCardManagerImpl.DATA_PROVISION_OPEN);

			default:
				break;
			}
		}

		if (dataProvision != null) {
			switch (dataProvision) {
			case FULLFILLED:

				list.add(AlphaCardManagerImpl.DATA_PROVISION_FULFILLED);
				break;
			case INPROGRESS:

				list.add(AlphaCardManagerImpl.DATA_PROVISION_INPROGRESS);
				break;
			case NOTFULFILLED:

				list.add(AlphaCardManagerImpl.DATA_PROVISION_OPEN_INPROGRESS);
				break;
			case OPEN:

				list.add(AlphaCardManagerImpl.DATA_PROVISION_OPEN);

			default:
				break;
			}
		}

		if (showDeleted == null) {
			list.add(AlphaCardManagerImpl.NOT_DELETED);
		} else if (showDeleted == CardFilterDeleted.NOTDELETED) {
			list.add(AlphaCardManagerImpl.NOT_DELETED);
		} else if (showDeleted == CardFilterDeleted.DELETED) {
			list.add(AlphaCardManagerImpl.DELETED);
		}

		if (list.isEmpty())
			return apCase.getAlphaCards();
		else
			return this.alphaCardManager.listAlphaCardsByCriterion(
					apCase.getCaseId(), list.toArray(new Criterion[0]));
	}

	/**
	 * handles the case, if the user clicks on one of the buttons.
	 * 
	 * @param myCase
	 *            the my case
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
	@SuppressWarnings("unchecked")
	@RequestMapping(method = RequestMethod.POST)
	public String onSubmit(final AlphaCase myCase, final BindingResult errors,
			final HttpServletRequest request, final HttpServletResponse response)
			throws Exception {

		this.log.fatal("This fallback Method should not be called");

		final Enumeration params = request.getParameterNames();
		while (params.hasMoreElements()) {
			this.log.error(params.nextElement().toString());
		}

		return "redirect:/caseform?caseId=" + myCase.getCaseId();
	}

	/**
	 * Adds the case.
	 * 
	 * @param alphaCase
	 *            the alpha case
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "addCase" })
	public String addCase(AlphaCase alphaCase, final BindingResult errors,
			final HttpServletRequest request, final HttpServletResponse response)
			throws Exception {

		final User currentUser = this.userManager.getUserByUsername(request
				.getRemoteUser());
		alphaCase.addParticipant(currentUser);
		alphaCase = this.caseManager.save(alphaCase);

		this.saveMessage(request,
				this.getText("case.added", request.getLocale()));
		return "redirect:/caseform?caseId=" + alphaCase.getCaseId();

	}

	/**
	 * Save case.
	 * 
	 * @param jspCase
	 *            the jsp case
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "saveCase" })
	public String saveCase(final AlphaCase jspCase, final BindingResult errors,
			final HttpServletRequest request, final HttpServletResponse response)
			throws Exception {

		AlphaCase alphaCase = this.caseManager.get(jspCase.getCaseId());

		if (!StringUtils.isBlank(jspCase.getName())) {
			alphaCase.setName(jspCase.getName());
			alphaCase = this.caseManager.save(alphaCase);
		}

		final String cardPriority = request
				.getParameter(CaseFormController.CARD_PRIORITY_PARAM_NAME);
		if (!StringUtils.isBlank(cardPriority)) {
			this.caseManager.updateCardOrder(alphaCase,
					Arrays.asList(StringUtils.split(cardPriority, ';')));
		}

		this.saveMessage(request,
				this.getText("case.updated", request.getLocale()));
		String redirect = "redirect:/caseform?caseId=" + alphaCase.getCaseId();
		final String activeCardId = request.getParameter("activeCardId");
		if (!StringUtils.isBlank(activeCardId)) {
			redirect += "&activeCardId=" + activeCardId;
		}
		return redirect;
	}

	/**
	 * Cancel case.
	 * 
	 * @param alphaCase
	 *            the alpha case
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "cancelCase" })
	public String cancelCase(final AlphaCase alphaCase,
			final BindingResult errors, final HttpServletRequest request)
			throws Exception {
		return this.getCancelView();
	}

	/**
	 * Delete case.
	 * 
	 * @param alphaCase
	 *            the alpha case
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "deleteCase" })
	public String deleteCase(final AlphaCase alphaCase,
			final BindingResult errors, final HttpServletRequest request)
			throws Exception {
		this.caseManager.remove(alphaCase.getCaseId());
		this.saveMessage(request,
				this.getText("case.deleted", request.getLocale()));
		return this.getCancelView();
	}

}
