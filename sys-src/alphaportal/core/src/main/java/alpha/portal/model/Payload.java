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
package alpha.portal.model;

import java.io.Serializable;

import javax.activation.MimeType;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Lob;

/**
 * the payload does not contain medical information. It contains only process
 * information.
 */
@Entity(name = "payload")
public class Payload implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The payload identifier. */
	@EmbeddedId
	private PayloadIdentifier payloadIdentifier;

	/** The filename. */
	private String filename;

	/** The mime type. */
	private String mimeType;

	/**
	 * The content of the payload.
	 */
	@Lob
	private byte[] content;

	/**
	 * The default constructor (used by Hibernate).
	 */
	public Payload() {
		this(null, null);
	}

	/**
	 * Creates a new Payload object with the given {@code filename} and
	 * {@code mimeType}.
	 * 
	 * @param filename
	 *            the files name
	 * @param mimeType
	 *            the files mimeType
	 * 
	 * @see #setContent(byte[])
	 */
	public Payload(final String filename, final String mimeType) {
		this.payloadIdentifier = new PayloadIdentifier();
		this.filename = filename;
		this.mimeType = mimeType;
	}

	/**
	 * gets the filename.
	 * 
	 * @return filename
	 */
	public String getFilename() {
		return this.filename;
	}

	/**
	 * sets the filename.
	 * 
	 * @param filename
	 *            filename, which has to be set
	 */
	public void setFilename(final String filename) {
		this.filename = filename;
	}

	/**
	 * gets the MimeType.
	 * 
	 * @return the mimeType
	 * @see MimeType MimeType
	 */
	public String getMimeType() {
		return this.mimeType;
	}

	/**
	 * sets the MimeType.
	 * 
	 * @param mimeType
	 *            the mimeType to set
	 */
	public void setMimeType(final String mimeType) {
		this.mimeType = mimeType;
	}

	/**
	 * Gets the id of the payload.
	 * 
	 * @return id
	 */
	public PayloadIdentifier getPayloadIdentifier() {
		return this.payloadIdentifier;
	}

	/**
	 * New payload identifier.
	 * 
	 * @param id
	 *            the new payload identifier
	 */
	public void setPayloadIdentifier(final PayloadIdentifier id) {
		this.payloadIdentifier = id;
	}

	/**
	 * Gets the content.
	 * 
	 * @return content
	 */
	public byte[] getContent() {
		return this.content;
	}

	/**
	 * Sets the content.
	 * 
	 * @param content
	 *            new content
	 */
	public void setContent(final byte[] content) {
		this.content = content;
	}

}
