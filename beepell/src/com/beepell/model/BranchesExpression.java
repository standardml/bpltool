package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * <p>
 * Java class for tBranches complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tBranches&quot;&gt;
 *   &lt;simpleContent&gt;
 *     &lt;extension base=&quot;&lt;http://docs.oasis-open.org/wsbpel/2.0/process/executable&gt;tExpression&quot;&gt;
 *       &lt;attribute name=&quot;successfulBranchesOnly&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; default=&quot;no&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/simpleContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tBranches")
public class BranchesExpression extends Expression {

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean successfulBranchesOnly;

    /**
     * Gets the value of the successfulBranchesOnly property.
     * 
     * @return possible object is {@link String }
     */
    public boolean isSuccessfulBranchesOnly() {
        if (successfulBranchesOnly == null) {
            return new Adapter1().unmarshal("no");
        } else {
            return successfulBranchesOnly;
        }
    }

    /**
     * Sets the value of the successfulBranchesOnly property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setSuccessfulBranchesOnly(Boolean value) {
        this.successfulBranchesOnly = value;
    }

}
