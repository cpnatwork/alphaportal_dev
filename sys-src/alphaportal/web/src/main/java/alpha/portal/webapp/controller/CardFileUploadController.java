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
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import org.appfuse.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.Payload;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.PayloadManager;

/**
 * Controller class to upload Files.
 * <p/>
 * <p>
 * <a href="FileUploadFormController.java.html"><i>View Source</i></a>
 * </p>
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */

@Controller
@RequestMapping("/cardfileupload*")
public class CardFileUploadController extends BaseFormController {

	/**
	 * the AlphaCardManager.
	 * 
	 * @see AlphaCardManager AlphaCardManager
	 */
	@Autowired
	AlphaCardManager alphaCardManager;

	/** The payload manager. */
	@Autowired
	PayloadManager payloadManager;

	/**
	 * default constructor.
	 */
	public CardFileUploadController() {

	}

	/**
	 * shows the card file upload site.
	 * 
	 * @param request
	 *            the request
	 * @return FileUpload
	 * @see cardfileupload.jsp
	 */
	@ModelAttribute
	@RequestMapping(method = RequestMethod.GET)
	public FileUpload showForm(final HttpServletRequest request) {

		final String caseId = request.getParameter("case");
		final String cardId = request.getParameter("card");
		final Locale locale = request.getLocale();

		request.setAttribute("case", caseId);
		request.setAttribute("card", cardId);

		this.setCancelView("redirect:/caseform?caseId=" + caseId
				+ "&activeCardId=" + cardId);
		this.setSuccessView("redirect:/caseform?caseId=" + caseId
				+ "&activeCardId=" + cardId);

		return new FileUpload();
	}

	/**
	 * handles the case, if the user clicks on one of the buttons.
	 * 
	 * @param fileUpload
	 *            the file upload
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @return success view
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	@RequestMapping(method = RequestMethod.POST)
	public String onSubmit(final FileUpload fileUpload,
			final BindingResult errors, final HttpServletRequest request)
			throws IOException {

		final String caseId = request.getParameter("case");
		final String cardId = request.getParameter("card");
		final Locale locale = request.getLocale();

		this.setCancelView("redirect:/caseform?caseId=" + caseId
				+ "&activeCardId=" + cardId);
		this.setSuccessView("redirect:/caseform?caseId=" + caseId
				+ "&activeCardId=" + cardId);

		final AlphaCard card = this.alphaCardManager
				.get(new AlphaCardIdentifier(caseId, cardId));
		if (card == null) {
			this.saveError(request, this.getText("card.invalidId", locale));
			return this.getCancelView();
		}
		final Adornment contributor = card.getAlphaCardDescriptor()
				.getAdornment(AdornmentType.Contributor.getName());

		if ((contributor.getValue() == null)
				|| contributor.getValue().isEmpty()) {

			this.saveError(request, this.getText("adornment.noAccess", locale));
			return this.getCancelView();

		} else {

			final Long contributorID = Long.parseLong(contributor.getValue());
			final User currentUser = this.getUserManager().getUserByUsername(
					request.getRemoteUser());

			if (contributorID != currentUser.getId()) {

				this.saveError(request,
						this.getText("adornment.noAccess", locale));
				return this.getCancelView();
			}
		}

		if (request.getParameter("cancel") != null)
			return this.getCancelView();

		if (this.validator != null) { // validator is null during testing
			fileUpload.setName("alphaCardPayloadFile");

			this.validator.validate(fileUpload, errors);

			if (errors.hasErrors())
				return "redirect:/cardfileupload?card=" + cardId + "&case="
						+ caseId;
		}

		// validate a file was entered
		if (fileUpload.getFile().length == 0) {
			final Object[] args = new Object[] { this.getText(
					"uploadForm.file", request.getLocale()) };
			errors.rejectValue("file", "errors.required", args, "File");

			return "redirect:/cardfileupload?card=" + cardId + "&case="
					+ caseId;
		}

		final MultipartHttpServletRequest multipartRequest = (MultipartHttpServletRequest) request;
		final MultipartFile file = multipartRequest.getFile("file");

		Payload payload = new Payload(file.getOriginalFilename(),
				file.getContentType());
		payload.setContent(file.getBytes());

		payload = this.payloadManager.saveNewPayload(payload, card);

		this.saveMessage(request, this.getText("card.payloadOK", locale));
		return this.getSuccessView();
	}
}
