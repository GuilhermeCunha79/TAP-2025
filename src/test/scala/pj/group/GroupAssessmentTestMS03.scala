package pj.group

import pj.assessment.{AssessmentBehaviours, AssessmentMS03}

class GroupAssessmentTestMS03 extends AssessmentBehaviours:
  performAllTests(AssessmentMS03.create, "files/group/ms03", "Milestone 3")
