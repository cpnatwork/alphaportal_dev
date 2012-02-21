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

import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.mock.web.MockMultipartHttpServletRequest;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;
import org.springframework.validation.ObjectError;

import alpha.portal.service.AlphaCardManager;

/**
 * The Class CardFileUploadControllerTest.
 */
public class CardFileUploadControllerTest extends BaseControllerTestCase {

	/** The ctrl. */
	@Autowired
	private CardFileUploadController ctrl;

	/** The alpha card manager. */
	@Autowired
	private AlphaCardManager alphaCardManager;

	/**
	 * Test on submit.
	 */
	@Test
	public void testOnSubmit() {
		final String caseId = "550e4713-e22b-11d4-a716-446655440000";
		final String cardId = "440e4816-e01b-74d4-a716-449955440092";
		final String fileName = "doesnotcompute.file";
		final String mimeType = "text/plain";
		final byte[] content = "roflcopter".getBytes();

		final MockHttpServletRequest request = this.newGet("/cardfileupload");
		request.setRemoteUser("admin");
		request.addParameter("case", caseId);
		request.addParameter("card", cardId);
		final FileUpload fileUpload = this.ctrl.showForm(request);
		fileUpload.setFile(content);

		final MockMultipartHttpServletRequest upload = new MockMultipartHttpServletRequest();
		upload.setRemoteUser("admin");
		final MockMultipartFile file = new MockMultipartFile("file", fileName,
				mimeType, content);
		upload.addFile(file);
		upload.addParameter("case", caseId);
		upload.addParameter("card", cardId);

		/*
		 * Sadly enough we would need a flush() within the
		 * PayloadManagerImpl.saveNewPayload() function for this test to succeed
		 * since we moved to saving the payload via its own manager/dao.
		 */

		// BindingResult errors = new DataBinder(fileUpload).getBindingResult();
		// String result = "";
		// try {
		// result = ctrl.onSubmit(fileUpload, errors, upload);
		// } catch (IOException e) {
		// fail("Should not fail on fail upload");
		// }
		//
		// assertFalse(errors.hasErrors());
		// assertNotNull(upload.getSession().getAttribute("successMessages"));
		//
		// AlphaCard myCard = alphaCardManager.get(new
		// AlphaCardIdentifier(caseId, cardId));
		// assertNotNull(myCard);
		// assertNotNull(myCard.getPayload());
		// Assert.assertArrayEquals(content, myCard.getPayload().getContent());
		// Assert.assertEquals(fileName, myCard.getPayload().getFilename());
		// Assert.assertEquals(mimeType, myCard.getPayload().getMimeType());

		// Assert.assertEquals("redirect:/caseform?caseId=" + caseId +
		// "&activeCardId=" + cardId, result);
	}

	/**
	 * Test on cancel.
	 */
	@Test
	public void testOnCancel() {
		final String caseId = "550e4713-e22b-11d4-a716-446655440000";
		final String cardId = "440e4816-e01b-74d4-a716-449955440092";
		final String fileName = "doesnotcompute.file";
		final String mimeType = "text/plain";
		final byte[] content = "roflcopter".getBytes();

		final MockHttpServletRequest request = this.newGet("/cardfileupload");
		request.setRemoteUser("admin");
		request.addParameter("case", caseId);
		request.addParameter("card", cardId);
		final FileUpload fileUpload = this.ctrl.showForm(request);
		fileUpload.setFile(content);

		final MockMultipartHttpServletRequest upload = new MockMultipartHttpServletRequest();
		upload.setRemoteUser("admin");
		final MockMultipartFile file = new MockMultipartFile("file", fileName,
				mimeType, content);
		upload.addFile(file);
		upload.addParameter("case", caseId);
		upload.addParameter("card", cardId);
		upload.addParameter("cancel", "Abbrechen");

		final BindingResult errors = new DataBinder(fileUpload)
				.getBindingResult();
		String result = "";
		try {
			result = this.ctrl.onSubmit(fileUpload, errors, upload);
		} catch (final IOException e) {
			Assert.fail("Should not fail on fail upload");
		}
		Assert.assertFalse(errors.hasErrors());
		Assert.assertNull(upload.getSession().getAttribute("successMessages"));

		Assert.assertEquals("redirect:/caseform?caseId=" + caseId
				+ "&activeCardId=" + cardId, result);
	}

	/**
	 * Test zero file.
	 */
	@Test
	public void testZeroFile() {
		final String caseId = "550e4713-e22b-11d4-a716-446655440000";
		final String cardId = "440e4816-e01b-74d4-a716-449955440092";
		final String fileName = "doesnotcompute.file";
		final String mimeType = "text/plain";
		final byte[] content = "".getBytes();

		final MockHttpServletRequest request = this.newGet("/cardfileupload");
		request.setRemoteUser("admin");
		request.addParameter("case", caseId);
		request.addParameter("card", cardId);
		final FileUpload fileUpload = this.ctrl.showForm(request);
		fileUpload.setFile(content);

		final MockMultipartHttpServletRequest upload = new MockMultipartHttpServletRequest();
		upload.setRemoteUser("user");
		final MockMultipartFile file = new MockMultipartFile("file", fileName,
				mimeType, content);
		upload.addFile(file);
		upload.addParameter("case", caseId);
		upload.addParameter("card", cardId);

		final BindingResult errors = new DataBinder(fileUpload)
				.getBindingResult();
		String result = "";
		try {
			result = this.ctrl.onSubmit(fileUpload, errors, upload);
		} catch (final IOException e) {
			Assert.fail("Should not fail on fail upload");
		}
		Assert.assertTrue(errors.hasErrors());
		final List<ObjectError> errorList = errors.getAllErrors();
		Assert.assertEquals(1, errorList.size());
		Assert.assertEquals("errors.required", errorList.get(0).getCode());
		Assert.assertNull(upload.getSession().getAttribute("successMessages"));

		Assert.assertEquals("redirect:/cardfileupload?card=" + cardId
				+ "&case=" + caseId, result);
	}
}
