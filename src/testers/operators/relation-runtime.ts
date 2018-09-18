import { BinaryOperatorRuntime } from '../../common/runtimes';

export type RelPlain<T> = BinaryOperatorRuntime<T, T, boolean>;
export type RelSel<T> = BinaryOperatorRuntime<string, T, boolean>
              | BinaryOperatorRuntime<T, string, boolean>;
export type RelationRuntime<T> = RelPlain<T> | RelSel<T>;
