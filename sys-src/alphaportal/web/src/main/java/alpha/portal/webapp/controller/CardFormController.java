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

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.FileCopyUtils;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeDeleted;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.CaseManager;

/**
 * Controller of the card form.
 * 
 * @see BaseFormController Base FormController
 */
@Controller
@RequestMapping("/cardform*")
public class CardFormController extends BaseFormController {

	/**
	 * the AlphaCardManager.
	 * 
	 * @see AlphaCardManager AlphaCardManager
	 */
	@Autowired
	private AlphaCardManager alphaCardManager;

	/**
	 * the CaseManager for setting the correct priority when creating.
	 * 
	 * @see CaseManager
	 */
	@Autowired
	private CaseManager caseManager;

	/**
	 * the userManager.
	 * 
	 * @see UserManager UserManager
	 */
	@Autowired
	private UserManager userManager;

	/**
	 * shows the card form.
	 * 
	 * @param request
	 *            the request
	 * @return ModelView
	 * @see cardform.jsp
	 */
	@SuppressWarnings("unchecked")
	@ModelAttribute("alphacard")
	@RequestMapping(method = RequestMethod.GET)
	protected String showForm(final HttpServletRequest request) {

		this.log.fatal("This fallback Method should not be called");

		final Enumeration params = request.getParameterNames();
		while (params.hasMoreElements()) {
			this.log.error(params.nextElement().toString());
		}

		return "redirect:/caseMenu";
	}

	/**
	 * handles the case, if the user clicks on one of the buttons.
	 * 
	 * @param myCard
	 *            the my card
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return success view
	 * @throws Exception
	 *             the exception
	 */
	@SuppressWarnings("unchecked")
	@RequestMapping(method = RequestMethod.POST)
	public String onSubmit(final AlphaCard myCard, final BindingResult errors,
			final HttpServletRequest request, final HttpServletResponse response)
			throws Exception {

		this.log.fatal("This fallback Method should not be called");

		final Enumeration params = request.getParameterNames();
		while (params.hasMoreElements()) {
			this.log.error(params.nextElement().toString());
		}

		return "redirect:/caseform?caseId="
				+ myCard.getAlphaCardIdentifier().getCaseId();
	}

	/**
	 * Save card.
	 * 
	 * @param jspCard
	 *            the jsp card
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
	@RequestMapping(method = RequestMethod.POST, params = { "saveCard" })
	public String saveCard(final AlphaCard jspCard, final BindingResult errors,
			final HttpServletRequest request, final HttpServletResponse response)
			throws Exception {
		AlphaCard alphaCard = null;
		AlphaCardIdentifier identifier = null;
		final Locale locale = request.getLocale();

		if (!StringUtils.isBlank(jspCard.getAlphaCardIdentifier().getCardId())) {
			alphaCard = this.alphaCardManager.get(jspCard
					.getAlphaCardIdentifier());
			identifier = alphaCard.getAlphaCardIdentifier();

			final String cardId = identifier.getCardId();
			final String caseId = identifier.getCaseId();

			final Adornment contributor = alphaCard.getAlphaCardDescriptor()
					.getAdornment(AdornmentType.Contributor.getName());

			if ((contributor.getValue() == null)
					|| contributor.getValue().isEmpty()) {

				this.saveError(request,
						this.getText("adornment.noAccess", locale));
				return "redirect:/caseform?activeCardId=" + cardId + "&caseId="
						+ caseId;

			} else {

				final Long contributorID = Long.parseLong(contributor
						.getValue());
				final User currentUser = this.getUserManager()
						.getUserByUsername(request.getRemoteUser());

				if (contributorID != currentUser.getId()) {
					this.saveError(request,
							this.getText("adornment.noAccess", locale));
					return "redirect:/caseform?activeCardId=" + cardId
							+ "&caseId=" + caseId;
				}
			}

			/**
			 * Check if card has been deleted
			 */
			final Adornment delAdornment = alphaCard.getAlphaCardDescriptor()
					.getAdornment(AdornmentType.Deleted.getName());
			if (delAdornment != null) {
				final String delIsTrue = AdornmentTypeDeleted.TRUE.value();
				if (delAdornment.getValue().equals(delIsTrue)) {
					this.saveError(request,
							this.getText("adornment.errorChange", locale));
					return "redirect:/caseform?activeCardId=" + cardId
							+ "&caseId=" + caseId;
				}
			}

			alphaCard.getAlphaCardDescriptor().setTitle(
					jspCard.getAlphaCardDescriptor().getTitle());

		} else {
			alphaCard = this.alphaCardManager.createAlphaCard(jspCard
					.getAlphaCardIdentifier().getCaseId());
			final User currentUser = this.userManager.getUserByUsername(request
					.getRemoteUser());
			alphaCard.getAlphaCardDescriptor().setContributor(
					currentUser.getId());
			for (final Adornment a : jspCard.getAlphaCardDescriptor()
					.getAllAdornments()) {
				alphaCard.getAlphaCardDescriptor().setAdornment(a.getName(),
						a.getValue());
			}
			identifier = alphaCard.getAlphaCardIdentifier();
			final AlphaCase alphaCase = this.caseManager.get(identifier
					.getCaseId());
			alphaCase.addAlphaCard(alphaCard);
		}

		alphaCard = this.alphaCardManager.save(alphaCard);

		this.saveMessage(request, this.getText("card.updated", locale));
		return "redirect:/caseform?caseId=" + identifier.getCaseId()
				+ "&activeCardId=" + identifier.getCardId();
	}

	/**
	 * Cancel card.
	 * 
	 * @param alphaCard
	 *            the alpha card
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "cancelCard" })
	public String cancelCard(final AlphaCard alphaCard,
			final BindingResult errors, final HttpServletRequest request)
			throws Exception {
		return "redirect:/caseform?caseId="
				+ alphaCard.getAlphaCardIdentifier().getCaseId();
	}

	/**
	 * Assign card.
	 * 
	 * @param jspCard
	 *            the jsp card
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "assignToMe" })
	public String assignCard(final AlphaCard jspCard,
			final BindingResult errors, final HttpServletRequest request)
			throws Exception {
		final AlphaCard alphaCard = this.alphaCardManager.get(jspCard
				.getAlphaCardIdentifier());
		final User currentUser = this.userManager.getUserByUsername(request
				.getRemoteUser());
		alphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Contributor.getName(),
				currentUser.getId().toString());

		this.alphaCardManager.save(alphaCard);

		final AlphaCardIdentifier identifier = alphaCard
				.getAlphaCardIdentifier();
		return "redirect:/caseform?caseId=" + identifier.getCaseId()
				+ "&activeCardId=" + identifier.getCardId();
	}

	/**
	 * Unassign card.
	 * 
	 * @param jspCard
	 *            the jsp card
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "unassignMe" })
	public String unassignCard(final AlphaCard jspCard,
			final BindingResult errors, final HttpServletRequest request)
			throws Exception {

		final AlphaCard alphaCard = this.alphaCardManager.get(jspCard
				.getAlphaCardIdentifier());
		alphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Contributor.getName(), "");

		this.alphaCardManager.save(alphaCard);

		final AlphaCardIdentifier identifier = alphaCard
				.getAlphaCardIdentifier();
		return "redirect:/caseform?caseId=" + identifier.getCaseId()
				+ "&activeCardId=" + identifier.getCardId();
	}

	/**
	 * Gets the payload.
	 * 
	 * @param jspCard
	 *            the jsp card
	 * @param response
	 *            the response
	 * @return the payload
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "payloadGet" })
	public String getPayload(final AlphaCard jspCard,
			final HttpServletResponse response) throws IOException {
		final AlphaCard alphaCard = this.alphaCardManager.get(jspCard
				.getAlphaCardIdentifier());
		final Payload payload = alphaCard.getPayload();

		if (payload != null) {

			final BufferedInputStream in = new BufferedInputStream(
					new ByteArrayInputStream(payload.getContent()));

			response.setBufferSize(payload.getContent().length);
			response.setContentType(payload.getMimeType());
			response.setHeader("Content-Disposition", "attachment; filename=\""
					+ payload.getFilename() + "\"");
			response.setContentLength(payload.getContent().length);

			FileCopyUtils.copy(in, response.getOutputStream());
			in.close();
			response.getOutputStream().flush();
			response.getOutputStream().close();
		}

		final AlphaCardIdentifier identifier = alphaCard
				.getAlphaCardIdentifier();
		return "redirect:/caseform?caseId=" + identifier.getCaseId()
				+ "&activeCardId=" + identifier.getCardId();
	}

	/**
	 * Delete payload.
	 * 
	 * @param jspCard
	 *            the jsp card
	 * @param request
	 *            the request
	 * @return the string
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "payloadDelete" })
	public String deletePayload(final AlphaCard jspCard,
			final HttpServletRequest request) throws IOException {
		final AlphaCard alphaCard = this.alphaCardManager.get(jspCard
				.getAlphaCardIdentifier());
		alphaCard.setPayload(null);
		this.alphaCardManager.save(alphaCard);

		final Locale locale = request.getLocale();
		this.saveMessage(request, this.getText("card.updated", locale));

		final AlphaCardIdentifier identifier = alphaCard
				.getAlphaCardIdentifier();
		return "redirect:/caseform?caseId=" + identifier.getCaseId()
				+ "&activeCardId=" + identifier.getCardId();
	}

	/**
	 * Sets the a card deleted.
	 * 
	 * @param jspCard
	 *            the jsp card
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "setDeleted" })
	public String setACardDeleted(final AlphaCard jspCard,
			final HttpServletRequest request) throws Exception {
		return this.setACardDeletedStatus(jspCard, request);
	}

	/**
	 * Sets the a card not deleted.
	 * 
	 * @param jspCard
	 *            the jsp card
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "setNotDeleted" })
	public String setACardNotDeleted(final AlphaCard jspCard,
			final HttpServletRequest request) throws Exception {
		return this.setACardDeletedStatus(jspCard, request);
	}

	/**
	 * Sets the a card deleted status.
	 * 
	 * @param jspCard
	 *            the jsp card
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	private String setACardDeletedStatus(final AlphaCard jspCard,
			final HttpServletRequest request) throws Exception {

		final AlphaCard alphaCard = this.alphaCardManager.get(jspCard
				.getAlphaCardIdentifier());

		final Adornment contributor = alphaCard.getAlphaCardDescriptor()
				.getAdornment(AdornmentType.Contributor.getName());
		if ((contributor.getValue() == null)
				|| contributor.getValue().isEmpty()) {
			this.saveError(request,
					this.getText("adornment.noAccess", request.getLocale()));
			final String cardId = alphaCard.getAlphaCardIdentifier()
					.getCardId();
			final String caseId = alphaCard.getAlphaCardIdentifier()
					.getCaseId();
			return "redirect:/caseform?activeCardId=" + cardId + "&caseId="
					+ caseId;
		} else {
			final Long contributorID = Long.parseLong(contributor.getValue());
			final User currentUser = this.getUserManager().getUserByUsername(
					request.getRemoteUser());
			if (contributorID != currentUser.getId()) {
				this.saveError(request,
						this.getText("adornment.noAccess", request.getLocale()));
				final String cardId = alphaCard.getAlphaCardIdentifier()
						.getCardId();
				final String caseId = alphaCard.getAlphaCardIdentifier()
						.getCaseId();
				return "redirect:/caseform?activeCardId=" + cardId + "&caseId="
						+ caseId;
			}
		}

		Adornment deletedAdornment = null;
		if (alphaCard.getAlphaCardDescriptor().getAdornment(
				AdornmentType.Deleted.getName()) == null) {
			deletedAdornment = new Adornment(AdornmentType.Deleted.getName());
			deletedAdornment.setValue(AdornmentTypeDeleted.FALSE.value());
			alphaCard.getAlphaCardDescriptor().setAdornment(deletedAdornment);
		}

		deletedAdornment = alphaCard.getAlphaCardDescriptor().getAdornment(
				AdornmentType.Deleted.getName());

		if (request.getParameter("setDeleted") != null) {
			deletedAdornment.setValue(AdornmentTypeDeleted.TRUE.value());
			this.alphaCardManager.save(alphaCard);
			this.saveMessage(request,
					this.getText("card.deleted", request.getLocale()));
		} else if (request.getParameter("setNotDeleted") != null) {
			deletedAdornment.setValue(AdornmentTypeDeleted.FALSE.value());
			this.alphaCardManager.save(alphaCard);
			this.saveMessage(request,
					this.getText("card.updated", request.getLocale()));
		}

		final AlphaCardIdentifier identifier = alphaCard
				.getAlphaCardIdentifier();
		return "redirect:/caseform?caseId=" + identifier.getCaseId()
				+ "&activeCardId=" + identifier.getCardId();
	}
}
