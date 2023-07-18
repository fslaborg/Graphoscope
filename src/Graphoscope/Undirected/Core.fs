namespace Graphoscope.Undirected

module Core =
    type Result<'TSuccess,'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure