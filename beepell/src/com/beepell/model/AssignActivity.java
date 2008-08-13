package com.beepell.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * <p>
 * Java class for tAssign complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tAssign&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;choice maxOccurs=&quot;unbounded&quot;&gt;
 *           &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}copy&quot;/&gt;
 *         &lt;/choice&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;validate&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; default=&quot;no&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tAssign", propOrder = { "copy" })
public class AssignActivity extends Activity {

    protected List<Copy> copy;

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean validate;

    /**
     * Gets the value of the copy property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the copy property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getCopy().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list {@link Copy }
     * 
     * @return list of copy operations.
     */
    public List<Copy> getCopy() {
        if (copy == null) {
            copy = new ArrayList<Copy>();
        }
        return this.copy;
    }

    /**
     * Gets the value of the validate property.
     * 
     * @return possible object is {@link String }
     */
    public boolean isValidate() {
        if (validate == null) {
            return new Adapter1().unmarshal("no");
        } else {
            return validate;
        }
    }

    /**
     * Sets the value of the validate property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setValidate(Boolean value) {
        this.validate = value;
    }

}
