package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.Servo;

import opmodes.TwoAxisShoulderTask;
import team25core.Robot;
import team25core.RobotEvent;

@Autonomous(name = "Two Axis Shoulder Test")
public class TwoAxisShoulderTest extends Robot {

    Servo xAxis;
    Servo yAxis;
    TwoAxisShoulderTask shoulderTask;

    @Override
    public void handleEvent(RobotEvent e)
    {
        if (e instanceof TwoAxisShoulderTask.TwoAxisShoulderEvent) {
            /**
             * Done, do whatever next.
             */
        }
    }

    @Override
    public void init()
    {
        xAxis = hardwareMap.servo.get("jewelXAxis");
        yAxis = hardwareMap.servo.get("jewelYAxis");

        shoulderTask = new TwoAxisShoulderTask(this, xAxis, yAxis);
        shoulderTask.init();

        /**
         * The setDirection() call, in a game autonomous, would be called as a result
         * of a ColorThief event determining jewel color.
         */
        shoulderTask.setDirection(TwoAxisShoulderTask.ShoulderDirection.FORWARD);

    }

    @Override
    public void start()
    {
        addTask(shoulderTask);
    }
}
