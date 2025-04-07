# Cost-Effectiveness of a Once-Weekly Injectable for Type 2 Diabetes in Sweden

## Abstract

**Objective:**  
To evaluate the cost-effectiveness of a once-weekly injectable therapy for type 2 diabetes compared to standard care from both healthcare and societal perspectives, using simulated data based on the Swedish healthcare system.

## Methods

Using generated cost and QALY data, a cost-effectiveness analysis (CEA) was conducted in R. The model included:

- Analyses from healthcare and societal perspectives
- Bootstrapping with 1,000 iterations to generate uncertainty intervals
- One-way sensitivity analysis (OWSA)
- Cost-effectiveness acceptability curves (CEAC)
- Subgroup analyses by sex and age

The willingness-to-pay (WTP) threshold was set at **500,000 SEK per QALY**, based on Swedish practice.

## Results

From the **healthcare perspective**, the intervention produced an ICER of **50,739 SEK/QALY**, with an incremental cost of 542 SEK and incremental QALY gain of 0.0107.

From the **societal perspective**, the intervention was cost-saving with an ICER of **-101,603 SEK/QALY**, due to lower productivity loss and indirect costs.

Subgroup analysis showed that the intervention was more cost-effective for:
- **Males** compared to females
- **Younger adults** compared to those aged 62+

### Bootstrapped Results:
- Incremental QALY 95% CI: [-0.0294, 0.0541]
- CEAC shows ~72% probability of cost-effectiveness at 500,000 SEK

## Visual Outputs

### Cost-Effectiveness Plane
![CE Plane](./ce_plane.png)

### Cost-Effectiveness Acceptability Curve (CEAC)
![CEAC](./ceac.png)

### Tornado Plot (OWSA)
![Tornado Plot](./tornado.png)

## Conclusion

This hypothetical evaluation illustrates the application of R in conducting a robust cost-effectiveness analysis with full uncertainty modeling. The once-weekly injectable appears to be a cost-effective or even cost-saving intervention, depending on the perspective taken. Results highlight the importance of perspective, subgroup differences, and parameter sensitivity when informing market access or HTA decisions.

The project demonstrates proficiency in:
- Real-world economic evaluation methodology
- R programming for CEA
- Communicating health economics results with clarity

