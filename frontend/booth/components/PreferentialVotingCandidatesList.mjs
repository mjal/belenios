import CandidateWithCheckbox from "./CandidateWithCheckbox.mjs";
import Column from './PreferentialVotingColumn.mjs';

const { DragDropContext } = window.ReactBeautifulDnd;

const buildColumnLabel = (column, columnOrderIndex) => {
  return column.title ? column.title : `Préférence ${columnOrderIndex+1}`; // TODO: i18n
};

const PreferenceLevelCreatorButton = ({onClick}) => {
  return e(
    "div",
    {
      className: "preferential-voting-ui__level-creator noselect",
    },
    e(
      "span",
      {
        className: "preferential-voting-ui__level-creator__add-icon clickable",
        onClick: onClick,
        title: "+ Ajouter ici un niveau de préférence" // TODO: i18n
      },
      "+ Ajouter ici un niveau de préférence" // TODO: i18n
    )
  );
};
      
class PreferentialVotingApp extends React.Component {
  constructor(props) {
    super(props);
    const { initialData } = props;
    this.state = {
      ...initialData,
      createdColumnsCounter: initialData.columnOrder.length
    };
    this.onDragEnd = this.onDragEnd.bind(this);
    this.moveCandidate = this.moveCandidate.bind(this);
    this.deletePreferenceLevel = this.deletePreferenceLevel.bind(this);
    this.insertPreferenceLevel = this.insertPreferenceLevel.bind(this);
    this.render = this.render.bind(this);
    this.buildUserVoteForAllCandidatesInQuestion = this.buildUserVoteForAllCandidatesInQuestion.bind(this);
    this.saveUserVoteForAllCandidatesInQuestion = this.saveUserVoteForAllCandidatesInQuestion.bind(this);
  }
  buildUserVoteForAllCandidatesInQuestion() {
    let updatedVoteForCandidates = [];
    this.state.columnOrder.forEach((currentColumnId, currentColumnIndex) => {
      this.state.columns[currentColumnId].candidatesIds.forEach((currentCandidateId) => {
        let candidate = this.state.candidates[currentCandidateId];
        if (!candidate){
          alert("could not find candidate");
        }
        else {
          updatedVoteForCandidates[candidate.initialIndex] = (currentColumnId === 'not-ranked') ? undefined : currentColumnIndex;
        }
      });
    });
    return updatedVoteForCandidates;
  }
  saveUserVoteForAllCandidatesInQuestion() {
    const userVote = this.buildUserVoteForAllCandidatesInQuestion();
    this.props.dispatchUpdateUserVoteForQuestion({
      type: 'saveVoteForAllCandidatesInQuestion',
      user_vote_for_all_candidates_in_question: userVote
    });
  }
  moveCandidate(candidateId, sourceColumnId, destinationColumnId, sourceColumnCandidateIndex, destinationColumnCandidateIndex) {
    const sourceColumn = this.state.columns[sourceColumnId];
    const newSourceColumnCandidateIds = Array.from(sourceColumn.candidatesIds);
    newSourceColumnCandidateIds.splice(sourceColumnCandidateIndex, 1);
    if (sourceColumnId === destinationColumnId){
      newSourceColumnCandidateIds.splice(destinationColumnCandidateIndex, 0, candidateId);
    }
    const newSourceColumn = {
      ...sourceColumn,
      candidatesIds: newSourceColumnCandidateIds
    }
    let changedColumns = {};
    changedColumns[newSourceColumn.id] = newSourceColumn;
    if (sourceColumnId != destinationColumnId){
      const destinationColumn = this.state.columns[destinationColumnId];
      const newDestinationColumnCandidateIds = Array.from(destinationColumn.candidatesIds);
      newDestinationColumnCandidateIds.splice(destinationColumnCandidateIndex, 0, candidateId);
      const newDestinationColumn = {
        ...destinationColumn,
        candidatesIds: newDestinationColumnCandidateIds
      };
      changedColumns[newDestinationColumn.id] = newDestinationColumn;
    }
    
    const newState = {
      ...this.state,
      columns: {
        ...this.state.columns,
        ...changedColumns
      }
    };
    this.setState(newState);

    // replicate the local state change in the more global state
    const destinationColumnIndex = this.state.columnOrder.findIndex(element => element == destinationColumnId);
    if (candidateId in this.state.candidates && destinationColumnIndex > -1){
      const candidateIndex = this.state.candidates[candidateId].initialIndex;
      this.props.dispatchUpdateUserVoteForQuestion({
        type: 'saveVoteForCandidateInQuestion',
        candidate_index: candidateIndex,
        user_vote_for_candidate: destinationColumnIndex
      });
    }
    else {
      alert("candidate index not found or destination column index not found");
    }
  }
  deletePreferenceLevel(columnId) {
    const index = this.state.columnOrder.findIndex(element => element == columnId);
    if (index === undefined){
      console.log(`/!\\ column id ${columnId} not found in this.state.columnOrder`);
      return;
    }
    const newColumnOrder = Array.from(this.state.columnOrder);
    const newColumns = JSON.parse(JSON.stringify(this.state.columns));
    newColumnOrder.splice(index, 1);
    if (newColumns.hasOwnProperty(columnId)){
      delete newColumns[columnId];
    }
    else {
      console.log(`/!\\ could not remove ${columnId} because it was absent from newColumns`);
    }
    const newState = {
      ...this.state,
      columns: newColumns,
      columnOrder: newColumnOrder
    };
    this.setState(newState, this.saveUserVoteForAllCandidatesInQuestion);
  }
  insertPreferenceLevel(insertBeforeIndex) {
    const newColumnId = `column-${this.state.createdColumnsCounter}`;
    const newColumnOrder = Array.from(this.state.columnOrder);
    newColumnOrder.splice(insertBeforeIndex, 0, newColumnId);
    const newState = {
      ...this.state,
      columns: {
        ...this.state.columns,
        [newColumnId]: {
          'id': newColumnId,
          'candidatesIds': []
        }
      },
      columnOrder: newColumnOrder,
      createdColumnsCounter: this.state.createdColumnsCounter+1
    };
    this.setState(newState, this.saveUserVoteForAllCandidatesInQuestion);
  }
  onDragEnd(result) {
    const { destination, source, draggableId } = result;
    if (!destination){
      return;
    }
    if (destination.droppableId === source.droppableId && destination.index === source.index){
      return;
    }
    this.moveCandidate(draggableId, source.droppableId, destination.droppableId, source.index, destination.index);
  }
  render() {
    const allColumns = this.state.columnOrder.map((columnId, index) => { return {id: columnId, label: buildColumnLabel(this.state.columns[columnId], index)}; });
    const children = this.state.columnOrder.map(
      (columnId, index) => {
        if(!this.state.columns.hasOwnProperty(columnId)){
          console.log(`/!\\ Column ${columnId} is present at index ${index} in this.state.columnOrder, but is absent from this.state.columns`);
          return e("div");
        }
        const column = this.state.columns[columnId];
        const candidates = column.candidatesIds.map(candidateId => this.state.candidates[candidateId]);
        const otherColumns = Array.from(allColumns);
        otherColumns.splice(index, 1);
        return e(
          "div",
          null,
          e(
            PreferenceLevelCreatorButton,
            {
              onClick: () => {
                this.insertPreferenceLevel(index);
              }
            }
          ),
          e(
            Column,
            {
              key: column.id,
              column: column,
              candidates: candidates,
              label: buildColumnLabel(column, index),
              onClickDeleteButton: () => {
                const canDeleteColumn = candidates.length == 0;
                if (canDeleteColumn){
                  this.deletePreferenceLevel(column.id);
                }
                else {
                  alert("You can delete a level of preference only if it is empty. Please first move the candidates it contains to other preference levels."); // TODO: i18n
                }
              },
              otherColumns: otherColumns,
              onSelectCandidateDestinationColumn: (candidateId, sourceColumnCandidateIndex, destinationColumnId) => {
                this.moveCandidate(candidateId, column.id, destinationColumnId, sourceColumnCandidateIndex, this.state.columns[destinationColumnId].candidatesIds.length);
              }
            }
          )
        );
      }
    );
    return e(
      "div",
      {
        className: "preferential-voting-ui"
      },
      e(
        DragDropContext,
        {
          onDragEnd: this.onDragEnd
        },
        ...children
      )
    );
  }
}

PreferentialVotingApp.defaultProps = {
  initialData: {
    candidates: {
      'candidate-0': { id: 'candidate-0', initialIndex: 0, content: 'Charge my phone' },
      'candidate-1': { id: 'candidate-1', initialIndex: 1,  content: 'Cook dinner' },
      'candidate-2': { id: 'candidate-2', initialIndex: 2,  content: 'Go to the pub' },
    },
    columns: {
      'column-0': {
        id: 'column-0',
        candidatesIds: [],
      },
      'column-1': {
        id: 'column-1',
        candidatesIds: [],
      },
      'not-ranked': {
        id: 'not-ranked',
        title: 'Non classé',
        candidatesIds: ['candidate-0', 'candidate-1', 'candidate-2'],
      },
    },
    columnOrder: ['column-0', 'column-1', 'not-ranked'],
  },
  blankVoteIsAllowed: false,
  t: function(s){ return s; },
  currentCandidatesHavingAlertsForQuestion: [],
  dispatchUpdateUserVoteForQuestion: () => {}
};


function TranslatablePreferentialVotingBigCandidatesList({ identifierPrefix, candidates, blankVoteIsAllowed, renderedBlankVoteComponent, currentUserVoteForQuestion, currentCandidatesHavingAlertsForQuestion, dispatchUpdateUserVoteForQuestion, t }){
  /*
  TODO:
  - use currentUserVoteForQuestion to build initialData
  - show alerts using currentCandidatesHavingAlertsForQuestion
  - implement blank vote using blankVoteIsAllowed and renderedBlankVoteComponent
  */
  let initialData = {};
  const candidatesForInitialData = candidates.map((candidateLabel, candidateIndex) => {
    return {
      id: `${identifierPrefix}_candidate_${candidateIndex}`,
      content: candidateLabel,
      initialIndex: candidateIndex
    };
  });
  initialData.candidates = candidatesForInitialData.reduce((accumulator, currentValue) => {
    accumulator[currentValue.id] = currentValue;
    return accumulator;
  }, {});
  initialData.columns = {
    'column-0': {
      id: 'column-0',
      candidatesIds: [],
    },
    'column-1': {
      id: 'column-1',
      candidatesIds: [],
    },
    'not-ranked': {
      id: 'not-ranked',
      title: 'Non classé', // TODO: i18n
      candidatesIds: candidatesForInitialData.map(candidate => candidate.id),
    },
  };
  // Facilitate reordering of the columns
  initialData.columnOrder = ['column-0', 'column-1', 'not-ranked'];

  return e(
    "div",
    {
      className: "preferential-voting-ui-container"
    },
    e(
      PreferentialVotingApp,
      {
        initialData,
        dispatchUpdateUserVoteForQuestion
      }
    )
  );
}

function TranslatablePreferentialVotingCandidatesList({ identifierPrefix, candidates, blankVoteIsAllowed, currentUserVoteForQuestion, currentCandidatesHavingAlertsForQuestion, dispatchUpdateUserVoteForQuestion, t }){
  let renderedBlankVoteComponent = null;
  const candidateIndex = candidates.length;
  const userHasSelectedBlankVote = blankVoteIsAllowed && (currentUserVoteForQuestion.length > candidates.length) && (currentUserVoteForQuestion[candidateIndex] === 1) ? true : false;
  if (blankVoteIsAllowed){
    const blankVoteLabel = t("Blank vote");
    const identifier = `${identifierPrefix}_blank-vote`;
    const currentAlerts = currentCandidatesHavingAlertsForQuestion && currentCandidatesHavingAlertsForQuestion.includes(candidateIndex);
    const dispatchBlankVoteInQuestion = (blankVoteIsChecked) => {
      dispatchUpdateUserVoteForQuestion({
        type: 'saveBlankVoteInQuestion',
        blankVoteIsChecked,
      });
    };
    const commonProps = {
      candidateInfo: blankVoteLabel,
      checked: userHasSelectedBlankVote,
      id: identifier,
      key: candidateIndex,
      dispatchUpdateUserVoteForCandidateInQuestion: dispatchBlankVoteInQuestion,
      currentAlertsForCandidateInQuestion: currentAlerts,
      name: identifier
    };
    const blankVoteProps = {
      style: {
        margin: "50px auto 30px",
        maxWidth: "400px"
      }
    };
    renderedBlankVoteComponent = e(
      CandidateWithCheckbox,
      {
        ...commonProps,
        ...blankVoteProps
      }
    );
  }
  let cssClasses = "preferential-voting-candidates-list noselect";
  if (userHasSelectedBlankVote){
    cssClasses += " preferential-voting-candidates-list--blank-vote-is-selected";
  }
  return e(
    "div",
    {
      className: cssClasses
    },
    e(
      TranslatablePreferentialVotingBigCandidatesList,
      {
        identifierPrefix,
        candidates,
        blankVoteIsAllowed,
        renderedBlankVoteComponent,
        currentUserVoteForQuestion,
        currentCandidatesHavingAlertsForQuestion,
        dispatchUpdateUserVoteForQuestion,
        t
      }
    )
  );
}

TranslatablePreferentialVotingCandidatesList.defaultProps = {
  identifierPrefix: "question_1",
  candidates: [
    "Candidate 1",
    "Candidate 2",
    "Candidate 3"
  ],
  blankVoteIsAllowed: false,
  t: function(s){ return s; },
  currentCandidatesHavingAlertsForQuestion: [],
  dispatchUpdateUserVoteForQuestion: () => {}
};

const PreferentialVotingCandidatesList = ReactI18next.withTranslation()(TranslatablePreferentialVotingCandidatesList);

export { PreferentialVotingCandidatesList, TranslatablePreferentialVotingCandidatesList };
export default PreferentialVotingCandidatesList;
