package opmodes.LM3;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/5/2019.
 */
public class DorisSkybridgePath {
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

    public enum ArmLocation {
        ARM_STOWED,
        ARM_DEPLOYED,
    }

    DeadReckonPath[][] paths = new DeadReckonPath[2][2];

    public DorisSkybridgePath() {
        // BLUE ALLIANCE, BUILDING SIDE
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 5, -0.2); // move to foundation
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 26, -0.8); // move to foundation
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.2); // touch foundation
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000); // phantom segment for arms
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.4); // pull foundation back
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 120, -0.5); // turn the foundation into the building
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 13, -0.8);
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000);
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 13, -0.4);
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, 0.8);

        // BLUE ALLIANCE, LOADING SIDE
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 23, 0.8);
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 10, 0.2);
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 6, -0.5);
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 72, -0.5);
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 60, 0.8);
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[DorisSkybridgePath.AllianceColor.BLUE.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 20, -0.8);

        // RED ALLIANCE, BUILDING SIDE
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 5, -0.2); // move to foundation
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 26, -0.8); // move to foundation
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.2); // touch foundation
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000); // phantom segment for arms
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.5); // pull foundation back
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 120, 0.7); // turn the foundation into the building
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 13, -0.8); // push foundation against wall
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000); // phantom segment for arms
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 13, 0.4);
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 35, 0.8);

        // RED ALLIANCE, LOADING SIDE
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 23, 0.8);
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 10, 0.2);
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 6, -0.5);
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 72, 0.5);
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 60, 0.8);
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[DorisSkybridgePath.AllianceColor.RED.ordinal()][DorisSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 20, -0.8);
    }

    public DeadReckonPath getPath(DorisSkybridgePath.AllianceColor allianceColor, DorisSkybridgePath.StartingPosition startingPosition) {
        return paths[allianceColor.ordinal()][startingPosition.ordinal()];
    }
}
