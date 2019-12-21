/*
package test;


import com.qualcomm.robotcore.eventloop.opmode.Autonomous;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import team25core.Robot;
import team25core.RobotEvent;
import team25core.StoneDetectionTask;

@Autonomous(name = "Stones Detection Unit Test", group = "Team 5218")
public class StoneDetectionTest extends Robot {

    private final static String TAG = "holly";
    private Telemetry.Item stonePositionTlm;
    private Telemetry.Item stoneTlm;
    private Telemetry.Item stoneConfidTlm;
    private Telemetry.Item stoneTypeTlm;
    private Telemetry.Item numStonesSeenTlm;
    private Telemetry.Item skystoneIndexTlm;
    private final float NO_STONES_SEEN_FLOAT = (float) -1;
    private final int NO_STONES_SEEN_INT = -1;
    private final float FIRST_SKYSTONE_SEEN = (float) -1;

    private double confidence;
    private double left;
    private double type;
    private int numStonesSeen;
    private float foundSkystoneLeft = NO_STONES_SEEN_FLOAT;
    private float previousSkystoneLeft = FIRST_SKYSTONE_SEEN;
    private float currentSkystoneLeft = FIRST_SKYSTONE_SEEN;
    private int index = NO_STONES_SEEN_INT;
    StoneDetectionTask mdTask;
    private String depotColor = "RED";

    @Override
    public void handleEvent(RobotEvent e)
    {

    }

    public boolean currSkystoneIsBetter(float currentLeft, float previousLeft) {
        boolean isBetter = false;
        if (depotColor == "RED") {
            if (currentLeft > previousLeft) {
                isBetter = true;
            }
        } else {  // depotColor is BLUE
            if (currentLeft < previousLeft) {
                isBetter = true;
            }
        }
        return(isBetter);
    }

    public int checkSkystonePosition(List<Recognition> stones)
    {
        int foundSkystoneIndex = NO_STONES_SEEN_INT;
        numStonesSeen = stones.size();
        numStonesSeenTlm.setValue(numStonesSeen);

        for (Recognition recognition : stones) {
            index++;
            if (recognition.getLabel().equals(StoneDetectionTask.LABEL_SKY_STONE)) {
                if (previousSkystoneLeft == FIRST_SKYSTONE_SEEN){
                    foundSkystoneIndex = index;
                    foundSkystoneLeft = recognition.getLeft();
                } else { //not first skystone, 2nd
                    previousSkystoneLeft = foundSkystoneLeft;
                    currentSkystoneLeft = recognition.getLeft();
                    if (currSkystoneIsBetter(currentSkystoneLeft, previousSkystoneLeft)) {
                        foundSkystoneIndex = index;
                        foundSkystoneLeft = recognition.getLeft();
                    }
                }

            }
        }
        return(foundSkystoneIndex);
    }

    public void startStoneDetection()
    {
        mdTask = new StoneDetectionTask(this, "Webcam1") {
            @Override
            public void handleEvent(RobotEvent e) {
                int skystoneIndex = NO_STONES_SEEN_INT;
                StoneDetectionEvent event = (StoneDetectionEvent)e;
                //0 gives you the first stone on list of stones

                    /*
                    confidence = event.stones.get(0).getConfidence();
                    left = event.stones.get(0).getLeft();
                    RobotLog.ii(TAG, "Saw: " + event.kind + " Confidence: " + confidence);

                    stonePositionTlm = telemetry.addData("LeftOrigin", left);
                    stoneConfidTlm = telemetry.addData("Confidence", confidence);
                    stoneTypeTlm = telemetry.addData("StoneType",type);

                skystoneIndex = checkSkystonePosition(event.stones);
                skystoneIndexTlm.setValue(skystoneIndex);
            }
        };

        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(StoneDetectionTask.DetectionKind.EVERYTHING);
    }

    @Override
    public void init()
    {
        startStoneDetection();
        numStonesSeenTlm.addData("numStonesSeen", NO_STONES_SEEN_INT);
        skystoneIndexTlm.addData("skystone index", NO_STONES_SEEN_INT);
    }

    @Override
    public void start()
    {
        addTask(mdTask);
    }
}
*/
