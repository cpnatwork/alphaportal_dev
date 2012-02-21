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

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

import javax.servlet.http.HttpServletRequest;

import org.appfuse.Constants;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.multipart.MultipartHttpServletRequest;
import org.springframework.web.multipart.commons.CommonsMultipartFile;

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
@RequestMapping("/fileupload*")
public class FileUploadController extends BaseFormController {

	/**
	 * Instantiates a new file upload controller.
	 */
	public FileUploadController() {
		this.setCancelView("redirect:/mainMenu");
		this.setSuccessView("uploadDisplay");
	}

	/**
	 * Show form.
	 * 
	 * @return the file upload
	 */
	@ModelAttribute
	@RequestMapping(method = RequestMethod.GET)
	public FileUpload showForm() {
		return new FileUpload();
	}

	/**
	 * On submit.
	 * 
	 * @param fileUpload
	 *            the file upload
	 * @param errors
	 *            the errors
	 * @param request
	 *            the request
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST)
	public String onSubmit(final FileUpload fileUpload,
			final BindingResult errors, final HttpServletRequest request)
			throws Exception {

		if (request.getParameter("cancel") != null)
			return this.getCancelView();

		if (this.validator != null) { // validator is null during testing
			this.validator.validate(fileUpload, errors);

			if (errors.hasErrors())
				return "fileupload";
		}

		// validate a file was entered
		if (fileUpload.getFile().length == 0) {
			final Object[] args = new Object[] { this.getText(
					"uploadForm.file", request.getLocale()) };
			errors.rejectValue("file", "errors.required", args, "File");

			return "fileupload";
		}

		final MultipartHttpServletRequest multipartRequest = (MultipartHttpServletRequest) request;
		final CommonsMultipartFile file = (CommonsMultipartFile) multipartRequest
				.getFile("file");

		// the directory to upload to
		final String uploadDir = this.getServletContext().getRealPath(
				"/resources")
				+ "/" + request.getRemoteUser() + "/";

		// Create the directory if it doesn't exist
		final File dirPath = new File(uploadDir);

		if (!dirPath.exists()) {
			dirPath.mkdirs();
		}

		// retrieve the file data
		final InputStream stream = file.getInputStream();

		// write the file to the file specified
		final OutputStream bos = new FileOutputStream(uploadDir
				+ file.getOriginalFilename());
		int bytesRead;
		final byte[] buffer = new byte[8192];

		while ((bytesRead = stream.read(buffer, 0, 8192)) != -1) {
			bos.write(buffer, 0, bytesRead);
		}

		bos.close();

		// close the stream
		stream.close();

		// place the data into the request for retrieval on next page
		request.setAttribute("friendlyName", fileUpload.getName());
		request.setAttribute("fileName", file.getOriginalFilename());
		request.setAttribute("contentType", file.getContentType());
		request.setAttribute("size", file.getSize() + " bytes");
		request.setAttribute("location", dirPath.getAbsolutePath()
				+ Constants.FILE_SEP + file.getOriginalFilename());

		final String link = request.getContextPath() + "/resources" + "/"
				+ request.getRemoteUser() + "/";
		request.setAttribute("link", link + file.getOriginalFilename());

		return this.getSuccessView();
	}
}
