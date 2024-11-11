# MLdepressionclustering

```mermaid
stateDiagram-v2
 SCZandBPindividuals.R --> generate_groups.R
 generate_groups.R --> Bernoulli.R
 prepDemographics.R --> Bernoulli.R
 prepPRS.R --> Bernoulli.R
 Bernoulli.R --> Downstreamanalysis.R
 generate_groups.R --> HC.R
 prepDemographics.R --> HC.R
 prepPRS.R --> HC.R
 HC.R --> Downstreamanalysis.R
