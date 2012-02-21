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
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.appfuse.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.FileCopyUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.PayloadManager;

/**
 * Shows all payload versions; only to contributor!.
 */

@Controller
@RequestMapping("/payloadVersions*")
public class PayloadVersionsController extends BaseFormController {

	/** the PayloadManager. @see PayloadManager PayloadManager */
	@Autowired
	private PayloadManager payloadManager;

	/** the AlphaCardManager. @see AlphaCardManager AlphaCardManager */
	@Autowired
	private AlphaCardManager alphaCardManager;

	/**
	 * shows the list of payload versions.
	 * 
	 * @param request
	 *            the request
	 * @return ModelView
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.GET)
	public ModelAndView handleRequest(final HttpServletRequest request)
			throws Exception {

		final ModelAndView returnMaV = new ModelAndView();

		final User currentUser = this.getUserManager().getUserByUsername(
				request.getRemoteUser());
		final Locale locale = request.getLocale();

		if ((request.getParameter("card") == null)
				|| (request.getParameter("case") == null)) {
			this.saveError(request,
					this.getText("payloadVersions.cardNotFound", locale));
			returnMaV.addObject("isErrors", true);
			return returnMaV;
		}
		final String cardId = request.getParameter("card");
		final String caseId = request.getParameter("case");

		final AlphaCard currentCard = this.alphaCardManager
				.get(new AlphaCardIdentifier(caseId, cardId));
		if (currentCard == null) {
			this.saveError(request,
					this.getText("payloadVersions.cardNotFound", locale));
			returnMaV.addObject("isErrors", true);
			return returnMaV;
		}

		Long cardContributor = null;
		try {
			cardContributor = Long.parseLong(currentCard
					.getAlphaCardDescriptor()
					.getAdornment(AdornmentType.Contributor.getName())
					.getValue());
		} catch (final NumberFormatException e) {
			cardContributor = 0L;
		}
		final Long currentUserId = currentUser.getId();
		if (cardContributor != currentUserId) {
			this.saveError(request, this.getText("adornment.noAccess", locale));
			returnMaV.addObject("isErrors", true);
			return returnMaV;
		}

		final List<Payload> payloads = this.payloadManager
				.getAllVersions(currentCard.getPayload());
		if (payloads.size() < 1) {
			this.saveError(request,
					this.getText("payloadVersions.noPayloads", locale));
			returnMaV.addObject("isErrors", true);
			return returnMaV;
		}

		returnMaV.addObject("payloadList", payloads);
		returnMaV.addObject("cardName", currentCard.getAlphaCardDescriptor()
				.getTitle());
		returnMaV.addObject("caseId", currentCard.getAlphaCardIdentifier()
				.getCaseId());
		returnMaV.addObject("cardId", currentCard.getAlphaCardIdentifier()
				.getCardId());

		return returnMaV;
	}

	/**
	 * Gets the payload.
	 * 
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return the payload
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	@RequestMapping(method = RequestMethod.GET, params = { "seqNumber" })
	public String getPayload(final HttpServletRequest request,
			final HttpServletResponse response) throws IOException {
		final Locale locale = request.getLocale();
		if ((request.getParameter("card") == null)
				|| (request.getParameter("case") == null)) {
			this.saveError(request,
					this.getText("payloadVersions.cardNotFound", locale));
			return "";
		}
		final String cardId = request.getParameter("card");
		final String caseId = request.getParameter("case");

		final AlphaCard currentCard = this.alphaCardManager
				.get(new AlphaCardIdentifier(caseId, cardId));
		final Payload payload = this.payloadManager
				.getVersion(new PayloadIdentifier(currentCard.getPayload()
						.getPayloadIdentifier().getPayloadId(), Long
						.parseLong(request.getParameter("seqNumber"))));

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

		return "redirect:/payloadVersions?case="
				+ currentCard.getAlphaCardIdentifier().getCaseId() + "&card="
				+ currentCard.getAlphaCardIdentifier().getCardId();
	}
}
