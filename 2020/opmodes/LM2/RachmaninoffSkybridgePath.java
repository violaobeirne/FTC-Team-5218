package opmodes.LM2;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/5/2019.
 */
public class RachmaninoffSkybridgePath {
    public enum AllianceColor {
        BLUE,
        RED,
        DEFAULT;
    }

    public enum StartingPosition {
        BUILDING,
        LOADING,
        DEFAULT;
    }

    DeadReckonPath[][] paths = new DeadReckonPath[2][2];

    public RachmaninoffSkybridgePath() {
        // BLUE ALLIANCE, BUILDING SIDE
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 28, -0.8); // move to foundation
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.2); // touch foundation
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000); // phantom segment for arms
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.4); // pull foundation back
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 120, -0.5); // turn the foundation into the building
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, -0.8);
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000);
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 14, -0.4);
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 46, 0.8);

        // BLUE ALLIANCE, LOADING SIDE
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 23, 0.8);
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 10, 0.2);
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 6, -0.5);
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 72, -0.5);
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 60, 0.8);
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 20, -0.8);

        // RED ALLIANCE, BUILDING SIDE
        // doublecheck for phantom segment
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 28, -0.8); // move to foundation
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.2); // touch foundation
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000); // phantom segment for arms
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.4); // pull foundation back
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 130, 0.5); // turn the foundation into the building
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 15, -0.8); // pull foundation back
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 20, 0.4); // pull foundation back
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 43, 0.8); // pull foundation back

        // RED ALLIANCE, LOADING SIDE
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 23, 0.8);
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 10, 0.2);
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 6, -0.5);
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 72, 0.5);
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 60, 0.8);
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 20, -0.8);
    }

    public DeadReckonPath getPath(RachmaninoffSkybridgePath.AllianceColor allianceColor, RachmaninoffSkybridgePath.StartingPosition startingPosition) {
        return paths[allianceColor.ordinal()][startingPosition.ordinal()];
    }
}
