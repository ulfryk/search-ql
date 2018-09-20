import { Map } from 'immutable';

import { isEmptyRuntime } from './is-empty';
import { lengthRuntime } from './length';
import { testRuntime } from './test';

const builtInFunctions = Map({
  is_empty: isEmptyRuntime,
  length: lengthRuntime,
  test_function: testRuntime,
});

export {
  builtInFunctions,
  isEmptyRuntime,
  lengthRuntime,
  testRuntime,
};
