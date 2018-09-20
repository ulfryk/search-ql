import { FunctionRuntime, NodeEvaluation } from '../../model';

export const testRuntime: FunctionRuntime<boolean> = (values, ast) => () =>
  NodeEvaluation.ofBoolean(values, ast)(false);
