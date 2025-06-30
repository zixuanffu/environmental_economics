# Environmental Economics 2025 Spring

## Discussion Paper 1
- [Report](Report1/report_referee.pdf)
## Discussion Paper 2
- [Report](Report2/report_referee.pdf)
## Method Note
- [Note](MethodNote/note_method.pdf)
## Coding Exercise
We collaborated on the coding exercise. 
En Qi's report answers the questions directly, with estimation results. Zixuan's report contains some explanations on the methodology.  

|                   | En Qi   | Zixuan       |
| ----------------- | ------- | ------------ |
| 1. Descriptive    | Done    | Done         |
| 3. Identification | Done    | Done         |
| 4. Estimation     | Results | Explanations |

### En Qi's report
- [Report](CodingExercise/Report/report_enqi.pdf)
- [Code](CodingExercise/Code/notebook_enqi.ipynb)
### Zixuan's report
- [Report](CodingExercise/Report/report.pdf)
- [Code (Incomplete)](CodingExercise/Code/estimation.R): 
  - There is no zeros in estimating choice probabilities. Perhaps something is wrong in my construction. 
  - Predicted return very far off from truth? Estimate the equation for each subcrop?
  - Is it okay to restrict to only two paths in the construction of left-hand side variable?
  ```
  
       k
      / \
     k   a
      \ /
       a
    ``` 
  - $\theta_R$ is around 4e-5 which is really weird.
  - Recovered the fixed effect but not yet the switching cost parameter.
  - Estimation of static and myopic model are described but not implemented.
