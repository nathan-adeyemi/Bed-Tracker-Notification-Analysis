import os
import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd

def plot_repair_results(
    df: pd.DataFrame,
    style: str,
    size: tuple,
    axes_names: dict,
    plot_title: str | list,
    legend_title: str,
    dir: str,
    x_axis: str,
    analyses: str,
    splitting_var: str = None,
    scale_factor: float = None,
    y_axis: str | dict = None,
):
    
    try:
        if analyses == 'observation-density':
            sns.set_theme(style=style)
            fig, axes = plt.subplots(2, 1, figsize=eval(size) if isinstance(size, str) else size)
            
            # Get unique values of the "variable" column
            variables = df["variable"].unique()
            df[splitting_var] = df[splitting_var].astype('float').round(2).astype('category')
            
            handles, labels = [], []

            for i, (ax, variable) in enumerate(zip(axes, variables)):
                subset = df[df["variable"] == variable]
                sns.lineplot(data=subset, x=x_axis, y="value", hue=splitting_var,hue_order= subset[splitting_var].unique(), ax=ax)
                
                # Set plot title from cfg
                ax.set_title(plot_title[i] if isinstance(plot_title, list) else plot_title)
                ax.set(**axes_names)
                # Set legend title from cfg
                legend_title = legend_title
                handles, labels = ax.get_legend_handles_labels()
                ax.set_xlabel('')
                # Collect handles and labels from the first subplot only
                if i == len(variables)-1:
                    handles, labels = ax.get_legend_handles_labels()
                else:
                    ax.set_xticklabels([])  # Remove x-axis labels
                    
                ax.get_legend().remove()  # Remove legend from each subplot

            # Create a shared legend for the first subplot
            legend = fig.legend(handles, labels, loc='center left', bbox_to_anchor=(1.05, 0.5), title=legend_title)

            # Set x-axis title for the bottom subplot only
            legend.set_bbox_to_anchor((0.85, 0.5))
            axes[len(variables)-1].set_xlabel(axes_names.get('xlabel'))

            # Adjust layout to make space for the legend on the right
            plt.tight_layout(rect=[0, 0, 0.85, 1])

            
        elif analyses == "repair-budget-SA":
            
            #  Scale factor for the secondary y-axis            
            fig, ax1 = plt.subplots(figsize=(8, 6))

            # Plot Trajectory Log-Likelihood
            sns.lineplot(data=df[df['variable'] == y_axis.get('primary')], x=x_axis, y='value', ax=ax1, color='blue', label='Log-Likelihood')
            sns.scatterplot(data=df[df['variable'] == y_axis.get('primary')], x=x_axis, y='value', ax=ax1, color='black', marker='x')

            # Secondary y-axis for Solution Runtime
            ax2 = ax1.twinx()
            sns.lineplot(data=df[df['variable'] == y_axis.get('secondary')], x=x_axis, y='value', ax=ax2, color='red', label='Runtime')
            sns.scatterplot(data=df[df['variable'] == y_axis.get('secondary')], x=x_axis, y='value', ax=ax2, color='black', marker='o')

            # Adjusting the scale of the secondary y-axis
            ax2.set_ylim(max(df[y_axis.get('secondary')]) - df[y_axis.get('secondary')].max(), max(df[y_axis.get('secondary')]) * scale_factor)

            # Labels and titles
            ax1.set_xlabel('Repair Budget')
            ax1.set_ylabel('Log-Likelihood')
            ax2.set_ylabel('Runtime (sec)')
            ax1.set_yscale('log')

            # Legend
            lines, labels = ax1.get_legend_handles_labels()
            lines2, labels2 = ax2.get_legend_handles_labels()
            ax2.legend(lines + lines2, ['Log-Likelihood'] + ['Runtime'], loc='lower center', bbox_to_anchor=(0.5, -0.15), ncol=2)

        # Save the plot
        plot_path = os.path.join(dir, "results-plot.png")
        print("Results graph located at" + plot_path)
        plt.savefig(plot_path, dpi=750, bbox_inches="tight")
        plt.close()

        print(f"Plot saved to {plot_path}")

    except Exception as e:
        print(e)
