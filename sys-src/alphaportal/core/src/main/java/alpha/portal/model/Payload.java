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

    private static final long serialVersionUID = 1L;

    @EmbeddedId
    private PayloadIdentifier payloadIdentifier;

    private String filename;

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
     * Creates a new Payload object with the given {@code filename} and {@code
     * mimeType}.
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
        return filename;
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
     * gets the MimeType
     * 
     * @return the mimeType
     * 
     * @see MimeType MimeType
     */
    public String getMimeType() {
        return mimeType;
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
        return payloadIdentifier;
    }

    /**
     * New payload identifier.
     */
    public void setPayloadIdentifier(PayloadIdentifier id) {
        this.payloadIdentifier = id;
    }

    /**
     * Gets the content.
     * 
     * @return content
     */
    public byte[] getContent() {
        return content;
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
