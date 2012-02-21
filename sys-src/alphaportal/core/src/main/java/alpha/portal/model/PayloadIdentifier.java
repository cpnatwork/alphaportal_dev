package alpha.portal.model;

import java.io.Serializable;

import javax.persistence.Embeddable;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

@Embeddable
public class PayloadIdentifier implements Serializable {
    private static final long serialVersionUID = 1L;

    private long payloadId;
    private long sequenceNumber;

    public PayloadIdentifier() {
    }

    public PayloadIdentifier(long payloadId, long sequenceNUmber) {
        this.payloadId = payloadId;
        this.sequenceNumber = sequenceNUmber;
    }

    public long getPayloadId() {
        return payloadId;
    }

    public void setPayloadId(long payloadId) {
        this.payloadId = payloadId;
    }

    public long getSequenceNumber() {
        return sequenceNumber;
    }

    public void setSequenceNumber(long sequenceNumber) {
        this.sequenceNumber = sequenceNumber;
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(1947995799, 551833291).append(payloadId).append(sequenceNumber).toHashCode();
    }

    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof PayloadIdentifier)) {
            return false;
        }
        PayloadIdentifier castOther = (PayloadIdentifier) other;
        return new EqualsBuilder().append(payloadId, castOther.payloadId).append(sequenceNumber,
                castOther.sequenceNumber).isEquals();
    }

}
